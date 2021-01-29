# Chapter 30: When Things Go Wrong

## Reading Notes

- Generalised Algebraic Data Types
- Existential Quantification 

```hs
data MyException =
    forall e . (Show e, Typeable e) => MyException e
```

- At runtime, when an exception is thrown, it starts rolling back through the stack, looking for a `catch`. When it finds a `catch`, it checks to see what type of exception this `catch` catches. It calls `fromException` and `cast` to check if the type of the exception that got thrown matches the type of an exception we’re handling with the `catch`. A catch that handles a `SomeException` will match any type of exception, due to the flexibility of that type.

- Asynchronous exceptions: exceptions raised from a different thread than the one that’ll receive the error.
  - Async exceptions are helpful and manifest in less obvious ways in other language runtimes and ecosystems. Don’t try to catch everything; just let it die, and make sure you have a process supervisor and good logs. No execution is better than bad execution.