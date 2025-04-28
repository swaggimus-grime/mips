use log::info;

pub struct Tty(String);

impl Tty {
    pub fn new() -> Tty {
        Tty(String::new())
    }
    
    pub fn push_char(&mut self, c: char) {
        match c {
            '\n' => {
                if !self.0.is_empty() {
                    info!("TTY output: {}", self.0);
                }
                self.clear();
            },
            '\r' => {},
            _ => self.0.push(c)
        }
    }
    
    fn clear(&mut self) {
        self.0.clear();
    }
}