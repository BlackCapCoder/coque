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


| Command | Description                                                                      |
|---------|----------------------------------------------------------------------------------|
| print   | Pop from your stack and print to STDOUT                                          |
| que     | Pop from your stack, queue to your anti's queue                                  |
| push    | Pop from your stack, push to your anti's stack                                   |
| fork    | Fork off your current timeline. Your new anti will take your place               |
| <       | Dequeue from your queue, push to your stack                                      |
| >       | Pop from your stack, queue to your queue                                         |
| dup     | Duplicate the topmost element on your stack                                      |
| swap    | Swap the two topmost elements on your stack                                      |
| def     | Pop twice. Adds an alias from the first to the second element to your dictionary |
| undef   | Pop once. The popped element is removed from your dictionary                     |


## Evaluation

The code is read word by word and added to your queue in-order. You automatically dequeue
commands from your queue until it is empty, at which point you halt.


If a command is present in your dictionary, the associated operation will be evaluated.
Otherwise it is pushed to your stack.


Hello world:

~~~
world hello print print
~~~

Hello world using your anti:

~~~
< print que
< print que
hello
world
push
push
~~~

`< print que` will first move the command `print` from your queue to your
stack, preventing it from being evaluated, then add it to your anti's queue.


Hello world by cooperation:

~~~
< print que
hello world
print
push
~~~

You print `world`, your anti print `hello`. The output is `hello world` because
your anti printed first.

