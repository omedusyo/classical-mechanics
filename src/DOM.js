
export function make(tagName) {
  return () => document.createElement(tagName);
}

export function text(str) {
  return () => document.createTextNode(str);
}

export const appendChild = parentDOM => childDOM => () => {
  parentDOM.appendChild(childDOM);
  return null;
};

export function root() {
  return document.body;
}

export function getFirstExplicit(just, nothing, qs) {
  return () => {
    const maybeDOM = document.querySelector(qs);
    if (maybeDOM === null) {
      return nothing;
    } else {
      return just(maybeDOM)
    }
  };
}

export const setAttribute = elDOM => attrName => val => () => {
  elDOM.setAttribute(attrName, val);
  return null;
};

export const setTextContent = elDOM => str => () => {
  elDOM.textContent = str;
  return null;
};

export const attachClickEvent = elDOM => eff => () => {
  elDOM.addEventListener("click", eff);
  return null
};

export const attachChangeEvent = elDOM => k => () => {
  elDOM.addEventListener("change", e => {
    k(e.target.value)();
  });
  return null
};

export const replaceWith = oldDOM => newDOM => () => {
  oldDOM.replaceWith(newDOM);
  return null;
};

export const getCanvasContext = canvasDOM => () => {
  return canvasDOM.getContext("2d");
};
