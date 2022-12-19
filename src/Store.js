
// forall msg model . (model -> msg -> Effect model) -> Effect model -> Effect (Store msg)
export const make = update => initModel => () => {
  let model = initModel;
  return {
    getModel() {
      return model;
    },
    send(msg) {
      // console.log("Preparing to trigger msg", msg);
      setTimeout(() => {
        // TODO: Check that `this` is trully the store...
        model = update(this)(msg)(model)();
        // console.log("new model is", model);
      }, 0);
    },
  };
};

// forall msg . msg -> Store msg -> Effect Unit
export const send = msg => store => () => {
  store.send(msg);
  return null;
};

export const getModel = store => () => {
  return store.getModel();
};
