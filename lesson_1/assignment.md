Pass 1:

Let's write a simple fizzbuzz service.

It will take messages of the form
{isfizzbuzz, SOMENUMBER}
And will return messages of the form
```
{none, SOMENUMBER}
{fizz, SOMENUMBER}
{buzz, SOMENUMBER}
{fizzbuzz, SOMENUMBER}
```
depending on the appropriate response.

It will also take messages of the form
{stats}
and will return a field with:
{requests #requests_made}

Things you'll need to look at:
the ! syntax for sending messages
receive syntax for recieving messages
self()

HINT:
The interface specified is incomplete, and missing a vital piece of information.

Pass 2:

Imagine determining fizzbuzz was actually time
consuming. (Maybe we're consulting a remote fizzbuzz service.) We
wouldn't want to block the service waiting.

Alter the service to execute the fizbuzz request in a separately
created process. That process should both send the answer back to the
originator, and tell it's creator that it finished, and then exit.

Track the number of responses answered as well as the number of
requests made, and insert that in the status response.

