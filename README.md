# cl-gestures

Multitouch gestures, built on [MTIF](http://github.com/rmhsilva/mtif).

Work in progress, but zooming and scrolling are functional.

This library doesn't assume much about gestures. It makes it easy to define your
own gestures (see Custom Gestures below), and hook the provided ones into
whatever system you want.


## Gestures

Use gestures by defining `on-start`, `on-update` and `on-stop` methods for
whichever gesture you want. That's all, what you do with them is up to you. Each
gesture class has different slots that you'll probably want to access.


### Scrolling

Gesture: Two fingers moving horizontally or vertically.

Classes: `cl-gestures:scroll-x` and `cl-gestures:scroll-y`.

Slot: `scroll-distance`, starts at 0, increases as fingers move *right*.

Example:

```common-lisp
;; Assuming we have getters and setters for the current x position...
(let ((initial-x))
  (defmethod on-start ((inst scroll-x))
    (declare (ignore inst))
    (setf initial-x (get-pos-x)))
  (defmethod on-update ((inst scroll-x))
    (with-slots (scroll-distance) inst
      (set-pos-x (+ scroll-distance initial-x)))))
```

### Zoom

Gesture: Two fingers moving apart.

Class: `cl-gestures:zoom`.

Slot: `zoom`, starts at 1.0, increases as fingers move apart.

Example:

```common-lisp
(let ((initial-factor))
  (defmethod on-start ((inst zoom))
    (declare (ignore inst))
    (setf initial-factor (get-zoom)))
  (defmethod on-update ((inst zoom))
    (with-slots (zoom) inst
      (set-zoom (* initial-factor zoom)))))
```


## Gesture Implementation

Have a look at [gestures_impl.lisp][impl]. The general idea is that gestures
involve a number of fingers, and have three events associated with them: start,
update, and stop. The gesture is 'started' as soon as the required number of
fingers are present on the trackpad, gets 'updated' every time a new trackpad
frame arrives, and is 'stopped' when the fingers are removed.

Currently this means fingers can be used for multiple gestures. This is a good
thing, as, for example, it means you can scroll horizontally and vertically at
the same time. If you don't want that behaviour, handle the state at a higher
level.

[impl]: https://github.com/rmhsilva/cl-gestures/blob/master/gestures-imp.lisp


### Custom Gestures

The `defgesture` macro makes it easy to define simple gestures. For example,
here is the `scroll-y` gesture:

```common-lisp
(defgesture scroll-y
  "Two finger vertical scroll"
  :n-fingers 2
  :slots ((initial-y1 :initform 0)
          (initial-y2 :initform 0)
          (scroll-distance :initform 0))
  :start (lambda (f1 f2)
           (setf initial-y1 (mtif:finger-pos-y f1))
           (setf initial-y2 (mtif:finger-pos-y f2)))
  :update (lambda (f1 f2)
            (setf scroll-distance
                  (+ (- (mtif:finger-pos-y f1) initial-y1)
                     (- (mtif:finger-pos-y f2) initial-y2)))))
```

Key things to define
- the number of fingers required
- gesture-specific slots (properties)
- functions to be called when the gesture starts or updates

The `:start` and `:update` functions take `n-fingers` parameters, and have all
gesture-specific slots exposed to them.


## Notes

Some of the code here isn't necessarily tied to `MTIF`, and could be used with
other trackpad drivers. If there's enough interest, this might happen one day.

`zoom`, `scroll-x` and `scroll-y` are should probably be renamed -- they
actually define the gestures which are usually interpreted as zoom or scrolling,
respectively. However they don't have to be. They'd be better named something
like 'two-finger-spread' and 'two-finger-horiz/vert'.


## License

MIT
