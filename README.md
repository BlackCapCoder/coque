# coque

coque is a stack-based, interpreted programming language that is
perhaps best described through illustrations:

~~~
@---------->
<----------?
~~~

That `@` there is you. You are standing at the arrow of time;
the operations of your program are queued out in front of you along
the arrow- its point marks your final operation.


The `?` is anti-you. He is standing at the end of time and is
the one who are queuing the operations that you are evaluating.
He is writing your future and you are writing his.


The two of you are not actually moving towards each other, but
coincide at every point in time; the arrows simply illustrate the
chronological progression of time to each of you.


Example:

~~~
 --🥒---$--->
   ^    v
<-------🥒--
~~~

Suppose you are out for a jog and get hungry. You don't have anything to
eat, so you ask anti-you for a cucumber- he hands you one. You eat your cucumber,
finish your jog, and drop by the store to buy that cucumber you just ate so
that anti-you can give it to you in the past.


## Forking

Even though anti-you can be quite useful, sometimes you find him a bit annoying too.
Whether he ate half your cucumber or failed to mention the upcoming bad weather,
you might find yourself wishing you could swap him out for someone else.


`fork` let's you do just that:


~~~
    ^
    ||
    ||
    |v
 ---+------>
<----------
~~~

Fork will fling you off your timeline so you can author someone new to take your place!
Even though the new arrows are drawn next to each other, this all happens in a single unit
of time from the point of view of anti-you.


## Commands

IO:

| Command | Description                        |
|---------|------------------------------------|
| print   | Pop from stack and print to STDOUT |

Stack:

| Command | Description                                                   |
|---------|---------------------------------------------------------------|
| que     | Pop from stack and queue to anti queue                        |
| push    | Pop from stack and push to anti stack                         |
| fork    | Fork off from current timeline, the new anti will replace you |
| <       | Pop from stack and queue to queue                             |
| >       | Dequeue from queue, push to stack                             |
| dup     | Duplicate the topmost element on the stack                    |
| swap    | Swap the two topmost elements on the stack                    |

Interpreter:

| Command | Description                                                                                           |
|---------|-------------------------------------------------------------------------------------------------------|
| reify   | Push current interpreter                                                                              |
| deify   | Pop an interpreter and install it as the current interpreter                                          |
| extract | Pop a symbol, then an interpreter. Push the operation associated with that symbol in that interpreter |
| install | Pop a symbol, then an operation, then an interpreter. Push a new interpreter which is the same as the one given, except that the given symbol is associated with the given operation |
| get_parent | Pop an interpreter, push its parent interpreter |
| set_parent | Pop an interpreter i, then an interpreter j. Push a new interpreter which is the same as i, except that it's parent is j |
| perform | Pop an operation and execute it |
| null | Push the null interpreter. It is an error to interpret anything with the null interpreter |
| uniform | Pop an operation, push an interpreter where all symbols are associated with that operation |
| create | Pop an interpreter, then a string. Push a new operation defined by how that interpreter would interpret that string |
| expand | Pop an operation. It Pushes a string, then an interpreter such that interpreting the string with the interpreter is the same as evaluating the operation |


## Evaluation

The code is read word by word and added to your queue in-order. You automatically dequeue
commands from your queue until it is empty, at which point you halt.


With the initial interpreter, if a symbol is understood then its associated operation will be executed.
Otherwise the symbol is pushed to the stack.


Hello world:

~~~
world hello print print
~~~

Hello world using your anti:

~~~
> print que
> print que
hello
world
push
push
~~~

`> print que` will first move the command `print` from the queue to the
stack, preventing it from being executed, then add it to your anti queue.


Hello world by cooperation:

~~~
> print que
hello world
print
push
~~~

You print `world`, your anti print `hello`. The output is `hello world` because
your anti printed first.

