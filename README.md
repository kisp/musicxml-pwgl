[![Build Status](https://travis-ci.org/kisp/musicxml-pwgl.svg?branch=master)](https://travis-ci.org/kisp/musicxml-pwgl)

The complete code here at github is for development of the library. If you
just want to download it for installing it into PWGL, please go to the
[releases page](https://github.com/kisp/musicxml-pwgl/releases) and then
download a `musicxml-pwgl-<version>.tgz` or `musicxml-pwgl-<version>.zip` file from the
**Downloads** section of the latest release.

## test-db extraction for this branch

``` common-lisp
(dolist (tc (list-test-cases))
  (let ((id (sqlite-orm::store-object-id tc)))
    ;; (print (format nil "~3,'0D" id))
    (ensure-directories-exist (merge-pathnames (format nil "~3,'0D/" id) (probe-file "test-db/")))
    (let ((*default-pathname-defaults* (merge-pathnames (format nil "~3,'0D/" id) (probe-file "test-db/"))))
      (with-output-to-file (out "enp" :if-exists :supersede)
        (pprint (enp tc) out)
        (terpri out))
      (with-output-to-file (out "musicxml" :if-exists :supersede)
        (write-string (musicxml tc) out))
      (with-output-to-file (out "describe" :if-exists :supersede)
        (describe tc out))
      (when (slot-boundp tc 'MUSICXML-PWGL.TEST-DB:ENP-SCREEN-SHOT)
        (with-output-to-file (out "enp-screen-shot.png" :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
         (write-sequence (enp-screen-shot tc) out))))))
```
