//
// tags: option maybe some none branded-type
//

//
// Using branded types to create the data and value constructors.
//

type None = {
  readonly __tag: 'None';
};

const none: Option<never> = {
  __tag: 'None',
};

type Some<V> = {
  readonly __tag: 'Some';
  readonly val: V;
};

function some<V>(v: V): Option<V> {
  return {
    __tag: 'Some',
    val: v,
  };
}

type Option<T> = Some<T> | None;

export {
  type Option,
  type None,
  type Some,
  none,
  some,
};
