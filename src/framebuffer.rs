use crate::*;
const WIDTH_IN_U8: usize = SCREEN_WIDTH / 8;

pub type FrameBufferLayout = [[u8; WIDTH_IN_U8]; SCREEN_HEIGHT];
pub type FrameBuffer = [u8; WIDTH_IN_U8 * SCREEN_HEIGHT];

#[derive(Clone, Default)]
pub struct FrameBufferInternal {
    pub data: FrameBufferLayout,
}

impl AsRef<FrameBuffer> for FrameBufferInternal {
    #[inline(always)]
    fn as_ref<'a>(&'a self) -> &FrameBuffer {
        let self_data_ptr = &self.data as *const FrameBufferLayout as *const FrameBuffer;

        // safe as self.data has the same lifetime as self
        let res: &'a FrameBuffer;
        unsafe {
            res = &*self_data_ptr;
        };

        res
    }
}
