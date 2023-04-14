import Elysia from 'elysia';

export const _newElysia = function () {
  return new Elysia();
};

export const _listen =  (port) => (elysia) => {
  elysia.listen(port);
  return elysia
};

export const _state = (key) => (value) => (elysia) => {
  elysia.state(key, value);
  return elysia;
};

export const _decorate = function (key, value, elysia) {
  elysia.decorate(key, value);
  return elysia;
};

export const _get = (route) => (handler) => (elysia) => {
  elysia.get(route, handler);
  return elysia;
};
