
export const foreignRequestAnimationFrame = k => () => {
  window.requestAnimationFrame(timestamp => k(timestamp)())
};
