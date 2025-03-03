/// Apply the FIR filter to the given samples using the given phase and return the filtered result
pub fn filter(phase: u8, samples: [i16; 4]) -> i32 {
    let coeffs = FIR_COEFFS[phase as usize];

    let mut r = 0;

    for i in 0..4 {
        let s = samples[i] as i32;
        let c = coeffs[i] as i32;

        r += s * c;
    }

    r >> 15
}

/// SPU FIR filter: 4 taps, 256 phases
///
/// FIR coefficients taken from Mednafen. No$ seems to have a very similar table although I haven't
/// compared all entries to make sure that they're equal (and No$ orders by tap and then by phase,
/// Mefnafen does the opposite which makes more sense).
static FIR_COEFFS: [[i16; 4]; 256] = [
    [0x12c7, 0x59b3, 0x1307, -1],
    [0x1288, 0x59b2, 0x1347, -1],
    [0x1249, 0x59b0, 0x1388, -1],
    [0x120b, 0x59ad, 0x13c9, -1],
    [0x11cd, 0x59a9, 0x140b, -1],
    [0x118f, 0x59a4, 0x144d, -1],
    [0x1153, 0x599e, 0x1490, -1],
    [0x1116, 0x5997, 0x14d4, -1],
    [0x10db, 0x598f, 0x1517, -1],
    [0x109f, 0x5986, 0x155c, -1],
    [0x1065, 0x597c, 0x15a0, -1],
    [0x102a, 0x5971, 0x15e6, -1],
    [0x0ff1, 0x5965, 0x162c, -1],
    [0x0fb7, 0x5958, 0x1672, -1],
    [0x0f7f, 0x5949, 0x16b9, -1],
    [0x0f46, 0x593a, 0x1700, -1],
    [0x0f0f, 0x592a, 0x1747, 0x0000],
    [0x0ed7, 0x5919, 0x1790, 0x0000],
    [0x0ea1, 0x5907, 0x17d8, 0x0000],
    [0x0e6b, 0x58f4, 0x1821, 0x0000],
    [0x0e35, 0x58e0, 0x186b, 0x0000],
    [0x0e00, 0x58cb, 0x18b5, 0x0000],
    [0x0dcb, 0x58b5, 0x1900, 0x0000],
    [0x0d97, 0x589e, 0x194b, 0x0001],
    [0x0d63, 0x5886, 0x1996, 0x0001],
    [0x0d30, 0x586d, 0x19e2, 0x0001],
    [0x0cfd, 0x5853, 0x1a2e, 0x0001],
    [0x0ccb, 0x5838, 0x1a7b, 0x0002],
    [0x0c99, 0x581c, 0x1ac8, 0x0002],
    [0x0c68, 0x57ff, 0x1b16, 0x0002],
    [0x0c38, 0x57e2, 0x1b64, 0x0003],
    [0x0c07, 0x57c3, 0x1bb3, 0x0003],
    [0x0bd8, 0x57a3, 0x1c02, 0x0003],
    [0x0ba9, 0x5782, 0x1c51, 0x0004],
    [0x0b7a, 0x5761, 0x1ca1, 0x0004],
    [0x0b4c, 0x573e, 0x1cf1, 0x0005],
    [0x0b1e, 0x571b, 0x1d42, 0x0005],
    [0x0af1, 0x56f6, 0x1d93, 0x0006],
    [0x0ac4, 0x56d1, 0x1de5, 0x0007],
    [0x0a98, 0x56ab, 0x1e37, 0x0007],
    [0x0a6c, 0x5684, 0x1e89, 0x0008],
    [0x0a40, 0x565b, 0x1edc, 0x0009],
    [0x0a16, 0x5632, 0x1f2f, 0x0009],
    [0x09eb, 0x5609, 0x1f82, 0x000a],
    [0x09c1, 0x55de, 0x1fd6, 0x000b],
    [0x0998, 0x55b2, 0x202a, 0x000c],
    [0x096f, 0x5585, 0x207f, 0x000d],
    [0x0946, 0x5558, 0x20d4, 0x000e],
    [0x091e, 0x5529, 0x2129, 0x000f],
    [0x08f7, 0x54fa, 0x217f, 0x0010],
    [0x08d0, 0x54ca, 0x21d5, 0x0011],
    [0x08a9, 0x5499, 0x222c, 0x0012],
    [0x0883, 0x5467, 0x2282, 0x0013],
    [0x085d, 0x5434, 0x22da, 0x0015],
    [0x0838, 0x5401, 0x2331, 0x0016],
    [0x0813, 0x53cc, 0x2389, 0x0018],
    [0x07ef, 0x5397, 0x23e1, 0x0019],
    [0x07cb, 0x5361, 0x2439, 0x001b],
    [0x07a7, 0x532a, 0x2492, 0x001c],
    [0x0784, 0x52f3, 0x24eb, 0x001e],
    [0x0762, 0x52ba, 0x2545, 0x0020],
    [0x0740, 0x5281, 0x259e, 0x0021],
    [0x071e, 0x5247, 0x25f8, 0x0023],
    [0x06fd, 0x520c, 0x2653, 0x0025],
    [0x06dc, 0x51d0, 0x26ad, 0x0027],
    [0x06bb, 0x5194, 0x2708, 0x0029],
    [0x069b, 0x5156, 0x2763, 0x002c],
    [0x067c, 0x5118, 0x27be, 0x002e],
    [0x065c, 0x50da, 0x281a, 0x0030],
    [0x063e, 0x509a, 0x2876, 0x0033],
    [0x061f, 0x505a, 0x28d2, 0x0035],
    [0x0601, 0x5019, 0x292e, 0x0038],
    [0x05e4, 0x4fd7, 0x298b, 0x003a],
    [0x05c7, 0x4f95, 0x29e7, 0x003d],
    [0x05aa, 0x4f52, 0x2a44, 0x0040],
    [0x058e, 0x4f0e, 0x2aa1, 0x0043],
    [0x0572, 0x4ec9, 0x2aff, 0x0046],
    [0x0556, 0x4e84, 0x2b5c, 0x0049],
    [0x053b, 0x4e3e, 0x2bba, 0x004d],
    [0x0520, 0x4df7, 0x2c18, 0x0050],
    [0x0506, 0x4db0, 0x2c76, 0x0054],
    [0x04ec, 0x4d68, 0x2cd4, 0x0057],
    [0x04d2, 0x4d20, 0x2d33, 0x005b],
    [0x04b9, 0x4cd7, 0x2d91, 0x005f],
    [0x04a0, 0x4c8d, 0x2df0, 0x0063],
    [0x0488, 0x4c42, 0x2e4f, 0x0067],
    [0x0470, 0x4bf7, 0x2eae, 0x006b],
    [0x0458, 0x4bac, 0x2f0d, 0x006f],
    [0x0441, 0x4b5f, 0x2f6c, 0x0074],
    [0x042a, 0x4b13, 0x2fcc, 0x0078],
    [0x0413, 0x4ac5, 0x302b, 0x007d],
    [0x03fc, 0x4a77, 0x308b, 0x0082],
    [0x03e7, 0x4a29, 0x30ea, 0x0087],
    [0x03d1, 0x49d9, 0x314a, 0x008c],
    [0x03bc, 0x498a, 0x31aa, 0x0091],
    [0x03a7, 0x493a, 0x3209, 0x0096],
    [0x0392, 0x48e9, 0x3269, 0x009c],
    [0x037e, 0x4898, 0x32c9, 0x00a1],
    [0x036a, 0x4846, 0x3329, 0x00a7],
    [0x0356, 0x47f4, 0x3389, 0x00ad],
    [0x0343, 0x47a1, 0x33e9, 0x00b3],
    [0x0330, 0x474e, 0x3449, 0x00ba],
    [0x031d, 0x46fa, 0x34a9, 0x00c0],
    [0x030b, 0x46a6, 0x3509, 0x00c7],
    [0x02f9, 0x4651, 0x3569, 0x00cd],
    [0x02e7, 0x45fc, 0x35c9, 0x00d4],
    [0x02d6, 0x45a6, 0x3629, 0x00db],
    [0x02c4, 0x4550, 0x3689, 0x00e3],
    [0x02b4, 0x44fa, 0x36e8, 0x00ea],
    [0x02a3, 0x44a3, 0x3748, 0x00f2],
    [0x0293, 0x444c, 0x37a8, 0x00fa],
    [0x0283, 0x43f4, 0x3807, 0x0101],
    [0x0273, 0x439c, 0x3867, 0x010a],
    [0x0264, 0x4344, 0x38c6, 0x0112],
    [0x0255, 0x42eb, 0x3926, 0x011b],
    [0x0246, 0x4292, 0x3985, 0x0123],
    [0x0237, 0x4239, 0x39e4, 0x012c],
    [0x0229, 0x41df, 0x3a43, 0x0135],
    [0x021b, 0x4185, 0x3aa2, 0x013f],
    [0x020d, 0x412a, 0x3b00, 0x0148],
    [0x0200, 0x40d0, 0x3b5f, 0x0152],
    [0x01f2, 0x4074, 0x3bbd, 0x015c],
    [0x01e5, 0x4019, 0x3c1b, 0x0166],
    [0x01d9, 0x3fbd, 0x3c79, 0x0171],
    [0x01cc, 0x3f62, 0x3cd7, 0x017b],
    [0x01c0, 0x3f05, 0x3d35, 0x0186],
    [0x01b4, 0x3ea9, 0x3d92, 0x0191],
    [0x01a8, 0x3e4c, 0x3def, 0x019c],
    [0x019c, 0x3def, 0x3e4c, 0x01a8],
    [0x0191, 0x3d92, 0x3ea9, 0x01b4],
    [0x0186, 0x3d35, 0x3f05, 0x01c0],
    [0x017b, 0x3cd7, 0x3f62, 0x01cc],
    [0x0171, 0x3c79, 0x3fbd, 0x01d9],
    [0x0166, 0x3c1b, 0x4019, 0x01e5],
    [0x015c, 0x3bbd, 0x4074, 0x01f2],
    [0x0152, 0x3b5f, 0x40d0, 0x0200],
    [0x0148, 0x3b00, 0x412a, 0x020d],
    [0x013f, 0x3aa2, 0x4185, 0x021b],
    [0x0135, 0x3a43, 0x41df, 0x0229],
    [0x012c, 0x39e4, 0x4239, 0x0237],
    [0x0123, 0x3985, 0x4292, 0x0246],
    [0x011b, 0x3926, 0x42eb, 0x0255],
    [0x0112, 0x38c6, 0x4344, 0x0264],
    [0x010a, 0x3867, 0x439c, 0x0273],
    [0x0101, 0x3807, 0x43f4, 0x0283],
    [0x00fa, 0x37a8, 0x444c, 0x0293],
    [0x00f2, 0x3748, 0x44a3, 0x02a3],
    [0x00ea, 0x36e8, 0x44fa, 0x02b4],
    [0x00e3, 0x3689, 0x4550, 0x02c4],
    [0x00db, 0x3629, 0x45a6, 0x02d6],
    [0x00d4, 0x35c9, 0x45fc, 0x02e7],
    [0x00cd, 0x3569, 0x4651, 0x02f9],
    [0x00c7, 0x3509, 0x46a6, 0x030b],
    [0x00c0, 0x34a9, 0x46fa, 0x031d],
    [0x00ba, 0x3449, 0x474e, 0x0330],
    [0x00b3, 0x33e9, 0x47a1, 0x0343],
    [0x00ad, 0x3389, 0x47f4, 0x0356],
    [0x00a7, 0x3329, 0x4846, 0x036a],
    [0x00a1, 0x32c9, 0x4898, 0x037e],
    [0x009c, 0x3269, 0x48e9, 0x0392],
    [0x0096, 0x3209, 0x493a, 0x03a7],
    [0x0091, 0x31aa, 0x498a, 0x03bc],
    [0x008c, 0x314a, 0x49d9, 0x03d1],
    [0x0087, 0x30ea, 0x4a29, 0x03e7],
    [0x0082, 0x308b, 0x4a77, 0x03fc],
    [0x007d, 0x302b, 0x4ac5, 0x0413],
    [0x0078, 0x2fcc, 0x4b13, 0x042a],
    [0x0074, 0x2f6c, 0x4b5f, 0x0441],
    [0x006f, 0x2f0d, 0x4bac, 0x0458],
    [0x006b, 0x2eae, 0x4bf7, 0x0470],
    [0x0067, 0x2e4f, 0x4c42, 0x0488],
    [0x0063, 0x2df0, 0x4c8d, 0x04a0],
    [0x005f, 0x2d91, 0x4cd7, 0x04b9],
    [0x005b, 0x2d33, 0x4d20, 0x04d2],
    [0x0057, 0x2cd4, 0x4d68, 0x04ec],
    [0x0054, 0x2c76, 0x4db0, 0x0506],
    [0x0050, 0x2c18, 0x4df7, 0x0520],
    [0x004d, 0x2bba, 0x4e3e, 0x053b],
    [0x0049, 0x2b5c, 0x4e84, 0x0556],
    [0x0046, 0x2aff, 0x4ec9, 0x0572],
    [0x0043, 0x2aa1, 0x4f0e, 0x058e],
    [0x0040, 0x2a44, 0x4f52, 0x05aa],
    [0x003d, 0x29e7, 0x4f95, 0x05c7],
    [0x003a, 0x298b, 0x4fd7, 0x05e4],
    [0x0038, 0x292e, 0x5019, 0x0601],
    [0x0035, 0x28d2, 0x505a, 0x061f],
    [0x0033, 0x2876, 0x509a, 0x063e],
    [0x0030, 0x281a, 0x50da, 0x065c],
    [0x002e, 0x27be, 0x5118, 0x067c],
    [0x002c, 0x2763, 0x5156, 0x069b],
    [0x0029, 0x2708, 0x5194, 0x06bb],
    [0x0027, 0x26ad, 0x51d0, 0x06dc],
    [0x0025, 0x2653, 0x520c, 0x06fd],
    [0x0023, 0x25f8, 0x5247, 0x071e],
    [0x0021, 0x259e, 0x5281, 0x0740],
    [0x0020, 0x2545, 0x52ba, 0x0762],
    [0x001e, 0x24eb, 0x52f3, 0x0784],
    [0x001c, 0x2492, 0x532a, 0x07a7],
    [0x001b, 0x2439, 0x5361, 0x07cb],
    [0x0019, 0x23e1, 0x5397, 0x07ef],
    [0x0018, 0x2389, 0x53cc, 0x0813],
    [0x0016, 0x2331, 0x5401, 0x0838],
    [0x0015, 0x22da, 0x5434, 0x085d],
    [0x0013, 0x2282, 0x5467, 0x0883],
    [0x0012, 0x222c, 0x5499, 0x08a9],
    [0x0011, 0x21d5, 0x54ca, 0x08d0],
    [0x0010, 0x217f, 0x54fa, 0x08f7],
    [0x000f, 0x2129, 0x5529, 0x091e],
    [0x000e, 0x20d4, 0x5558, 0x0946],
    [0x000d, 0x207f, 0x5585, 0x096f],
    [0x000c, 0x202a, 0x55b2, 0x0998],
    [0x000b, 0x1fd6, 0x55de, 0x09c1],
    [0x000a, 0x1f82, 0x5609, 0x09eb],
    [0x0009, 0x1f2f, 0x5632, 0x0a16],
    [0x0009, 0x1edc, 0x565b, 0x0a40],
    [0x0008, 0x1e89, 0x5684, 0x0a6c],
    [0x0007, 0x1e37, 0x56ab, 0x0a98],
    [0x0007, 0x1de5, 0x56d1, 0x0ac4],
    [0x0006, 0x1d93, 0x56f6, 0x0af1],
    [0x0005, 0x1d42, 0x571b, 0x0b1e],
    [0x0005, 0x1cf1, 0x573e, 0x0b4c],
    [0x0004, 0x1ca1, 0x5761, 0x0b7a],
    [0x0004, 0x1c51, 0x5782, 0x0ba9],
    [0x0003, 0x1c02, 0x57a3, 0x0bd8],
    [0x0003, 0x1bb3, 0x57c3, 0x0c07],
    [0x0003, 0x1b64, 0x57e2, 0x0c38],
    [0x0002, 0x1b16, 0x57ff, 0x0c68],
    [0x0002, 0x1ac8, 0x581c, 0x0c99],
    [0x0002, 0x1a7b, 0x5838, 0x0ccb],
    [0x0001, 0x1a2e, 0x5853, 0x0cfd],
    [0x0001, 0x19e2, 0x586d, 0x0d30],
    [0x0001, 0x1996, 0x5886, 0x0d63],
    [0x0001, 0x194b, 0x589e, 0x0d97],
    [0x0000, 0x1900, 0x58b5, 0x0dcb],
    [0x0000, 0x18b5, 0x58cb, 0x0e00],
    [0x0000, 0x186b, 0x58e0, 0x0e35],
    [0x0000, 0x1821, 0x58f4, 0x0e6b],
    [0x0000, 0x17d8, 0x5907, 0x0ea1],
    [0x0000, 0x1790, 0x5919, 0x0ed7],
    [0x0000, 0x1747, 0x592a, 0x0f0f],
    [-1, 0x1700, 0x593a, 0x0f46],
    [-1, 0x16b9, 0x5949, 0x0f7f],
    [-1, 0x1672, 0x5958, 0x0fb7],
    [-1, 0x162c, 0x5965, 0x0ff1],
    [-1, 0x15e6, 0x5971, 0x102a],
    [-1, 0x15a0, 0x597c, 0x1065],
    [-1, 0x155c, 0x5986, 0x109f],
    [-1, 0x1517, 0x598f, 0x10db],
    [-1, 0x14d4, 0x5997, 0x1116],
    [-1, 0x1490, 0x599e, 0x1153],
    [-1, 0x144d, 0x59a4, 0x118f],
    [-1, 0x140b, 0x59a9, 0x11cd],
    [-1, 0x13c9, 0x59ad, 0x120b],
    [-1, 0x1388, 0x59b0, 0x1249],
    [-1, 0x1347, 0x59b2, 0x1288],
    [-1, 0x1307, 0x59b3, 0x12c7],
];
