# coque

coque is a stack-based, interpreted programming language that is
perhaps best described through illustrations:

~~~
@---------->
<----------?
~~~

That `@` there is you. You are standing at the arrow of time;
the operations of your program are queued out in front of you along
the arrow, the point marking the final operation.


The `?` is anti-you. He is standing at the end of time, and is
the one who are queuing the operations that you are evaluating.
He is writing your future, and you are writing his.


The two of you are not actually moving towards each other- you
coincide at every point in time; the arrows simply mark the
chronological progression of time to you.


Example:

~~~
 --🥒---$--->
   ^    v
<-------🥒--
~~~

Suppose you are out for a jog when you get hungry. You don't have anything to
eat, so you ask anti-you for a cucumber; he hands you one. You eat your cucumber,
finish your jog, and drop by the store to buy that cucumber that you just ate so
that anti-you can give it to you in the past.


## Bailing

Even though anti-you can be quite useful, sometimes you find him a bit annoying too.
Whether he ate half your cucumber or failed to mention the upcoming rain, you find
yourself wishing that you could swap him out for someone else.


The `bail` command will let you do that:


~~~
    ^
    ||
    ||
    |v
 ---+------>
<----------
~~~

Even though the new arrows are drawn side-by-side, this all happens in a single unit
of time from the point of view of the old anti-you. Bail lets you swing off your old
timeline and up to the right, authoring some new person to take your place!
