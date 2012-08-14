
# Features
- 
-

# Setup
If you use Emacs 23 or lower, you must install ert.

```lisp
(auto-install-from-url "https://raw.github.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el")
```

You can install el-spec from github.

```lisp
(auto-install-from-url "https://raw.github.com/uk-ar/el-spec/master/el-spec.el")
```
Or you can install from marmalade.

```
M-x package-install el-spec
```

# How to use

## Basic: Setup and Teardown

You can use "before/after" macro for writing setup or teardown.
Simple example is here.

```
(require 'el-spec)

(describe "description"
  (before
    (message "before common")
    )
  (after
    (message "after common\n")
    )
  (it "test 1"
    (message "test 1")
    )
  (it "test 2"
    (message "test 2")
    )
  )
```

Run the example.

```
before common
test 1
after common

before common
test 2
after common
```

To run examples, you can use "M-x el-spec:eval-buffer" or "eval-defun" on describe's S-exp.

## Basic: Using context

You can use "context" keyword for nesting setup and teardown.
Simple example is here.

```
(describe "description"
  (before
    (message "before common"))
  (after
    (message "after common\n"))
  (context "when 1"
    (before
      (message "before 1"))
    (after
      (message "after 1"))
    (it "test 1"
      (message "test 1")))
  (context "when 2"
    (before
      (message "before 2"))
    (after
      (message "after 2"))
    (it "test 2"
      (message "test 2")))
  )
```

Run the example.

```
before common
before 1
test 1
after 1
after common

before common
before 2
test 2
after 2
after common
```

You can also use "around" macro for wrapping example.
This example is same as following example.

```
(describe "description"
  (around
    (message "before common")
    (funcall el-spec:example)
    (message "after common\n")
    )
  (context "when 1"
    (around
      (message "before 1")
      (funcall el-spec:example)
      (message "after 1")
      )
    (it "test 1"
      (message "test 1")))
  (context "when 2"
    (around
      (message "before 2")
      (funcall el-spec:example)
      (message "after 2")
      )
    (it "test 2"
      (message "test 2")))
  )
```

It is usefull to use "around" macro for "with-temp-buffer" or "let".

## Advanced

Here is step by step tutorial.
Tutorial target is auto inserting paired parentheses.

The spec is
- Inserting paired parenthese, braces, quotes when input open character.
- Inserting only close pairs, when input close character.
- Discovers which characters to pair based on the currently active syntax table.

Simple example is here.

```
(with-temp-buffer
  (switch-to-buffer (current-buffer))
  (c-mode)
  (execute-kbd-macro "'")
  (should (string= (buffer-string) "''"))
  )

(with-temp-buffer
  (switch-to-buffer (current-buffer))
  (c-mode)
  (insert "'")
  (execute-kbd-macro "'")
  (should (string= (buffer-string) "''"))
  )

(with-temp-buffer
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  (execute-kbd-macro "'")
  (should (string= (buffer-string) "'"))
  )
```

You can use "around" macro for common use of "switch-to-buffer".

```
(describe "auto-pair"
  (around
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (funcall el-spec:example)))

  (it ()
    (c-mode)
    (execute-kbd-macro "'")
    (should (string= (buffer-string) "''")))
  (it ()
    (c-mode)
    (insert "'")
    (execute-kbd-macro "'")
    (should (string= (buffer-string) "''")))
  (it ()
    (emacs-lisp-mode)
    (execute-kbd-macro "'")
    (should (string= (buffer-string) "'")))
  )
```

You can omit description string for it.

## Advanced: Using variables

Let's make groups by major mode.

```
(describe "auto-pair"
  (around
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (funcall el-spec:example)))

  (context "in c-mode"
    (before
     (c-mode))

    (it ()
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''")))
    (it ()
      (insert "'")
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''"))))

  (context "in emacs-lisp-mode"
    (before
      (emacs-lisp-mode))

    (it ()
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "'"))))
  )
```

You can use variables for major mode name.

```
(describe ("auto-pair" :vars (mode))
  (around
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (funcall mode)
      (funcall el-spec:example)))

  (context ("in c-mode" :vars ((mode 'c-mode)))
    (it ()
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''")))
    (it ()
      (insert "'")
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''"))))

  (context "in emacs-lisp-mode"
    (let ((mode 'emacs-lisp-mode))
      (it ()
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "'")))))
  )
```

## Advanced: Shared context

Use shared_context to define a block that will be evaluated in the context of example groups either explicitly, using include_context, or implicitly by matching metadata.

For example, if you want to add examples in c-mode and emacs-lisp-mode.

```
(describe ("auto-pair" :vars (mode))
  (around
    ...)

  (context ("in c-mode" :vars ((mode 'c-mode)))
    (it ()
      ...)

    (context ("buffer-string before" :vars (string-of-buffer))
      (before
        (insert string-of-buffer))
      (it (:vars ((string-of-buffer "'")))
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "''")))))

  (context "in emacs-lisp-mode"
    (let ((mode 'emacs-lisp-mode))
      (it ()
        ...)

      (context ("buffer-string before" :vars (string-of-buffer))
        (before
          (insert string-of-buffer))
        (it (:vars ((string-of-buffer "\"")))
          (execute-kbd-macro "\"")
          (should (string= (buffer-string) "\"\"")))
        )))
  )
```

You can use shared-context and include-context for common use of behavior.

```
(describe ("auto-pair" :vars (mode))
  (around
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (funcall mode)
      (funcall el-spec:example)))
  (shared-context ("insert string" :vars (string-of-buffer))
    (before
      (insert string-of-buffer)))

  (context ("in c-mode" :vars ((mode 'c-mode)))
    (it ()
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''")))

    (context "buffer-string before"
      (include-context "insert string")
      (it (:vars ((string-of-buffer "'")))
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "''")))))

  (context "in emacs-lisp-mode"
    (let ((mode 'emacs-lisp-mode))
      (it ()
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "'")))

      (context "buffer-string before"
        (include-context "insert string")
        (it (:vars ((string-of-buffer "\"")))
          (execute-kbd-macro "\"")
          (should (string= (buffer-string) "\"\"")))
        )))
  )
```

## Advanced: Shared examples

Shared example groups let you describe behaviour of types or modules. When declared, a shared group's content is stored. It is only realized in the context of another example group, which provides any context the shared group needs to run.

A shared group is included in another group using the include_examples methods.

You can add examples in c-mode and emacs-lisp-mode.
```
(describe ("auto-pair" :vars (mode))
  (around
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (funcall mode)
      (funcall el-spec:example)))
  (shared-context ("insert string" :vars (string-of-buffer))
    (before
      (insert string-of-buffer)))
  (shared-examples "examples for \""
    (it ()
      (execute-kbd-macro "\"")
      (should (string= (buffer-string) "\"\""))))

  (context ("in c-mode" :vars ((mode 'c-mode)))
    (it ()
      (execute-kbd-macro "'")
      (should (string= (buffer-string) "''")))
    (include-examples "examples for \"")
    (context "buffer-string before"
      (include-context "insert string")
      (it (:vars ((string-of-buffer "'")))
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "''")))))

  (context "in emacs-lisp-mode"
    (let ((mode 'emacs-lisp-mode))
      (it ()
        (execute-kbd-macro "'")
        (should (string= (buffer-string) "'")))
      (include-examples "examples for \"")
      ))
  )
```

## Advanced: Changing execution range

You can execute a examples around the cursole with "M-x el-spec:eval-and-execute-examples".
You can execute all examples around the cursole with "M-x el-spec:eval-and-execute-all".
You can change default behavior of eval-defun with M-x el-spec:toggle-selection.

