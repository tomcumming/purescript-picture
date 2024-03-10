export function onResize({ canvas, action }) {
  return () => {
    const obs = new ResizeObserver((entries) => action());
    obs.observe(canvas);
    return () => obs.disconnect();
  };
}

export function onAnimationFrame(action) {
  return () => {
    window.requestAnimationFrame(action);
  };
}

export function getDimensions(canvas) {
  return () => ({
    width: canvas.clientWidth,
    height: canvas.clientHeight
  });
}

export function setCanvasPixels({ width, height, canvas }) {
  return () => {
    canvas.width = width;
    canvas.height = height;
  };
}

export function setTransform({ctx, tx}) {
  return () => {
    const { m11, m12, m21, m22, m41, m42 } = tx;
    ctx.setTransform(m11, m12, m21, m22, m41, m42);
  };
}

export function getContext(canvas) {
  return () => {
    const ctx = canvas.getContext('2d');
    if (!ctx) throw new Error(`Could not create canvas 2d context`);
    return ctx;
  };
}

export function setFillStyle({ style, ctx }) {
  return () => {
    ctx.fillStyle = style;
  }
}

export function fillPoly({ ctx, path }) {
  return () => {
    ctx.beginPath();
    for(const {x, y} of path.slice(0, 1)) {
      ctx.moveTo(x, y);
    }
    for(const {x, y} of path.slice(1)) {
      ctx.lineTo(x, y);
    }
    ctx.fill();
  }
}
