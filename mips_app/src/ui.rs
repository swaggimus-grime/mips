
use egui::{CentralPanel, Context, RawInput};
use egui_wgpu::ScreenDescriptor;

pub struct UI {
    pub ctx: Context,
}

impl UI {
    pub fn new() -> Self {
        Self {
            ctx: Context::default(),
        }
    }

    pub fn update(&mut self) {
        let mut screen_desc = ScreenDescriptor {
            size_in_pixels: [256, 240],
            pixels_per_point: 1.0,
        };
        let input = RawInput {
            // Add real mouse/keyboard input here later
            screen_rect: Some(egui::Rect::from_min_size(
                egui::Pos2::ZERO,
                egui::vec2(screen_desc.size_in_pixels[0] as f32, screen_desc.size_in_pixels[1] as f32),
            )),
            time: None,
            ..Default::default()
        };
        let _ = self.ctx.run(input, |ctx| {
            CentralPanel::default().show(ctx, |ui| {
                ui.heading("SDL3 + egui + wgpu");
                ui.label("This is a basic UI overlay.");
            });
        });
    }
}

