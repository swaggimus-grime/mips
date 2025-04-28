use crate::psx::mdec::idct_matrix::IdctMatrix;
use crate::psx::mdec::macroblock::{Macroblock, MacroblockCoeffs};
use crate::psx::mdec::util::quantize;

#[test]
fn test_quantize_dc() {
    // XXX These values are taken from Mednafen at the moment, it
    // would be better to validate against the real hardware.
    assert_eq!(quantize(0, 0, None), 0);
    assert_eq!(quantize(0, 255, None), 0);
    assert_eq!(quantize(1, 0, None), 32);
    assert_eq!(quantize(1, 1, None), 8);
    assert_eq!(quantize(1, 2, None), 24);
    assert_eq!(quantize(1, 255, None), 4072);
    assert_eq!(quantize(2, 0, None), 64);
    assert_eq!(quantize(5, 204, None), 16312);
    assert_eq!(quantize(5, 205, None), 16383);
    assert_eq!(quantize(5, 206, None), 16383);
    assert_eq!(quantize(5, 255, None), 16383);
    assert_eq!(quantize(512, 0, None), -16384);
    assert_eq!(quantize(512, 1, None), -8184);
    assert_eq!(quantize(512, 2, None), -16376);
    assert_eq!(quantize(589, 0, None), -13920);
    assert_eq!(quantize(589, 1, None), -6952);
    assert_eq!(quantize(589, 2, None), -13912);
    assert_eq!(quantize(1023, 255, None), -4072);
    assert_eq!(quantize(1023, 0, None), -32);
}

#[test]
fn test_quantize_ac() {
    // XXX These values are taken from Mednafen at the moment, it
    // would be better to validate against the real hardware.
    assert_eq!(quantize(0, 0, Some(0)), 0);
    assert_eq!(quantize(0, 0, Some(63)), 0);
    assert_eq!(quantize(0, 1, Some(1)), 0);
    assert_eq!(quantize(0, 255, Some(63)), 0);
    assert_eq!(quantize(1, 0, Some(0)), 32);
    assert_eq!(quantize(1, 1, Some(0)), 32);
    assert_eq!(quantize(1, 1, Some(1)), -8);
    assert_eq!(quantize(1, 1, Some(7)), -8);
    assert_eq!(quantize(1, 1, Some(8)), 8);
    assert_eq!(quantize(1, 1, Some(15)), 8);
    assert_eq!(quantize(1, 1, Some(16)), 24);
    assert_eq!(quantize(1, 39, Some(62)), 4824);
    assert_eq!(quantize(1, 255, Some(63)), 16383);
    assert_eq!(quantize(1, 255, Some(32)), 16312);
    assert_eq!(quantize(2, 0, Some(0)), 64);
    assert_eq!(quantize(511, 255, Some(63)), 16383);
    assert_eq!(quantize(512, 0, Some(0)), -16384);
    assert_eq!(quantize(1000, 0, Some(0)), -768);
    assert_eq!(quantize(1000, 2, Some(57)), -5464);
    assert_eq!(quantize(1000, 220, Some(27)), -16384);
    assert_eq!(quantize(1003, 80, Some(3)), -10072);
}

#[test]
fn test_idct() {
    let coeffs = MacroblockCoeffs {
        coeffs: [
            0, 257, 514, 771, 1028, 1285, 1542, 1799, 8, 265, 522, 779, 1036, 1293, 1550, 1807, 16,
            273, 530, 787, 1044, 1301, 1558, 1815, 24, 281, 538, 795, 1052, 1309, 1566, 1823, 32,
            289, 546, 803, 1060, 1317, 1574, 1831, 40, 297, 554, 811, 1068, 1325, 1582, 1839, 48,
            305, 562, 819, 1076, 1333, 1590, 1847, 56, 313, 570, 827, 1084, 1341, 1598, 1855,
        ],
    };

    // This is the "standard" IDCT table used in most PSX games
    let idct_coeffs: [i16; 64] = [
        23170, 23170, 23170, 23170, 23170, 23170, 23170, 23170, 32138, 27245, 18204, 6392, -6393,
        -18205, -27246, -32139, 30273, 12539, -12540, -30274, -30274, -12540, 12539, 30273, 27245,
        -6393, -32139, -18205, 18204, 32138, 6392, -27246, 23170, -23171, -23171, 23170, 23170,
        -23171, -23171, 23170, 18204, -32139, 6392, 27245, -27246, -6393, 32138, -18205, 12539,
        -30274, 30273, -12540, -12540, 30273, -30274, 12539, 6392, -18205, 27245, -32139, 32138,
        -27246, 18204, -6393,
    ];

    let mut matrix = IdctMatrix::new();

    for (i, b) in idct_coeffs.iter().enumerate() {
        // The "weird" bitshift used by mednafen
        matrix.set(i as u8, b >> 3);
    }

    let mut block = Macroblock::new();

    matrix.idct(&coeffs, &mut block);

    let expected = Macroblock {
        block: [
            -128, -95, 71, -27, 38, -5, 22, 9, 127, 96, -75, 27, -40, 4, -23, -10, 127, -39, 30,
            -11, 16, -2, 9, 4, -117, 33, -26, 9, -14, 1, -8, -3, 62, -18, 14, -5, 7, -1, 4, 2, -52,
            14, -11, 4, -6, 1, -4, -2, 21, -6, 5, -2, 3, 0, 1, 1, -14, 3, -3, 1, -1, 0, -1, 0,
        ],
    };

    for i in 0..64 {
        assert_eq!(expected.block[i], block.block[i]);
    }
}
