
// ===Canvas operations===

export const makePath2D = () => {
  return new Path2D();
};

export const stroke = path2d => ctx => () => {
  ctx.stroke(path2d)
  return null;
};

export const fill = path2d => ctx => () => {
  ctx.fill(path2d)
  return null;
};

export const setFillStyle = color => ctx => () => {
  ctx.fillStyle = color;
  return null;
};

export const setStrokeStyle = color => ctx => () => {
  ctx.strokeStyle = color;
  return null;
};

export const foreignClear = width => height => ctx => () => {
  ctx.clearRect(0, 0, width, height);
  return null;
};

export const save = ctx => () => {
  ctx.save();
  return null;
};

export const restore = ctx => () => {
  ctx.restore();
  return null;
};

export const setGlobalAlpha = alpha => ctx => () => {
  ctx.globalAlpha = alpha;
  return null;
};


// ===Path2D operations===
export const rect = x => y => width => height => path2d => () => {
  path2d.rect(x, y, width, height);
  return null
};

export const arc = x => y => radius => angle0 => angle1 => path2d => () => {
  path2d.arc(x, y, radius, angle0, angle1);
  return null
};

export const lineTo = x => y => path2d => () => {
  path2d.lineTo(x, y);
  return null
};

export const moveTo = x => y => path2d => () => {
  path2d.moveTo(x, y);
  return null
};
