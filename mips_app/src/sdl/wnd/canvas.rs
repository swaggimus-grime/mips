use std::ops::Deref;
use sdl3::rect::Rect;
use super::Window;

pub struct Canvas {
    pub canvas: sdl3::render::WindowCanvas,
}

impl From<&Window> for Canvas {
    fn from(window: &Window) -> Self {
        Canvas { canvas: window.wnd().clone().into_canvas() }
    }
}

impl Deref for Canvas {
    type Target = sdl3::render::WindowCanvas;
    fn deref(&self) -> &Self::Target {
        &self.canvas
    }
}

impl Canvas {
    pub fn clear(&mut self) {
        self.canvas.clear();
    }

    pub fn present(&mut self) {
        self.canvas.present();
    }

    pub fn copy(&mut self, texture: &sdl3::render::Texture) {
        self.canvas.copy(texture, None, None);
    }
    
    pub fn set_size(&mut self, width: u32, height: u32) {
        self.canvas.set_viewport(Rect::from((0, 0, width, height)));
    }
}