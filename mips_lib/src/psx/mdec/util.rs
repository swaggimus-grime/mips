/// Convert `val` into a signed 10 bit value
pub fn to_10bit_signed(val: u16) -> i16 {
    ((val << 6) as i16) >> 6
}

/// Quantization function for the macroblock coefficients. For the DC coeffs qscale should be None.
pub fn quantize(coef: u16, quantization: u8, qscale: Option<u8>) -> i16 {
    if coef == 0 {
        0
    } else {
        let c = to_10bit_signed(coef);
        let (qscale, qshift) = match qscale {
            Some(qs) => (qs, 3),
            // DC doesn't use the qscale value and does not require right shifting after the
            // multiplication.
            _ => (1, 0),
        };

        let q = quantization as i32 * qscale as i32;

        let c = if q == 0 {
            (c << 5) as i32
        } else {
            let c = c as i32;
            let c = (c * q) >> qshift;
            let c = c << 4;

            // XXX This is from mednafen, not sure why this is needed.
            if c < 0 {
                c + 8
            } else {
                c - 8
            }
        };

        // Saturate
        if c > 0x3fff {
            0x3fff
        } else if c < -0x4000 {
            -0x4000
        } else {
            c as i16
        }
    }
}

pub fn sign_extend_9bits_clamp_8bits(v: i32) -> i8 {
    let v = v as u16;
    let v = v << (16 - 9);
    let v = (v as i16) >> (16 - 9);

    // Saturate
    if v < -128 {
        -128
    } else if v > 127 {
        127
    } else {
        v as i8
    }
}

pub fn yuv_to_rgb(y: i8, u: i8, v: i8) -> (u8, u8, u8) {
    // XXX Taken from mednafen, not completely accurate.
    let y = y as i32;
    let u = u as i32;
    let v = v as i32;

    let r = y + (((359 * v) + 0x80) >> 8);
    let g = y + ((((-88 * u) & !0x1f) + ((-183 * v) & !0x07) + 0x80) >> 8);
    let b = y + (((454 * u) + 0x80) >> 8);

    let r = sign_extend_9bits_clamp_8bits(r) as u8 ^ 0x80;
    let g = sign_extend_9bits_clamp_8bits(g) as u8 ^ 0x80;
    let b = sign_extend_9bits_clamp_8bits(b) as u8 ^ 0x80;

    (r, g, b)
}
