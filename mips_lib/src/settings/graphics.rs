pub struct GraphicsSettings {
    vram_display_mode: VRamDisplayMode
}

impl Default for GraphicsSettings {
    fn default() -> GraphicsSettings {
        GraphicsSettings {
            vram_display_mode: Default::default()
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, Copy, Clone, PartialEq, Eq, Debug, Default)]
#[repr(u8)]
pub enum VRamDisplayMode {
    #[default]
    Native,
    Full16bpp,
    Full8bpp,
    Full4bpp,
}

impl VRamDisplayMode {
    pub fn max_res(self) -> (u16, u16) {
        match self {
            // Maximum resolution supported by the PlayStation video output is 640x576. That high a
            // vertical resolution would mean no blanking however, so it doesn't make a lot of
            // sense.
            VRamDisplayMode::Native => (640, 480),
            VRamDisplayMode::Full16bpp => (1024, 512),
            VRamDisplayMode::Full8bpp => (2048, 512),
            VRamDisplayMode::Full4bpp => (4096, 512),
        }
    }

    pub fn aspect_ratio(self) -> f32 {
        match self {
            VRamDisplayMode::Native => 4. / 3.,
            VRamDisplayMode::Full16bpp => 2. / 1.,
            VRamDisplayMode::Full8bpp => 4. / 1.,
            VRamDisplayMode::Full4bpp => 8. / 1.,
        }
    }
}