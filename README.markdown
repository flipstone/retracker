Retracker
=========


What is it?
-----------

Retracker is nothing more than a request forwarding and broadcast system
intended for use with Pivotal Tracker's Activity Webhook integeration.
Tracker only allows you to configure one endpoint url for Webhook
integration per project. If you want to push activity to multiple
endpoints, you're out of luck.

By pointing your Tracker project's Webhook integration at a Retracker
instance, you can broadcast Webhook posts to multiple endpoints. Retracker
can be configured with any (reasonable) number of endpoints, and it will
forwards incoming posts to all of them.


Installation
------------

You'll need Haskell platform (tested against 2011.2.0.1) with cabal.
We recommended installing cabal-dev before installing any packages, and
then using cabal-dev to install retracker, like this:

    $ cabal-install cabal-dev
    $ cd <retracker directory>
    $ cabal-install .

With retracker installed, first configure the destinations you want
posts to be forwarded to, then start it, like this:

    $ cd <retracker directory>
    $ vi resources/config/destinations

&lt;enter destinations urls, one per line&gt;

    $ ./cabal-dev/bin/retracker -p <port number>

This will start retracker on the given port number. Once retracker is
running, all posts to

    http://<domain>:<port>/retrack

Will be re-posted to the urls configured in the `desinations` file


A Few Details
-------------

Retracker accepts posts and puts them on an in-memory queue to be processed.
It will report a success back to the original poster befare this queue has
had a chance to process, so no forwarding errors will be propagated back.

The processing queue is processed serially for each configured destination.
This is, for a given destination url, Retracker will never make a second
post until the current one was returned. Each desination processes the
queue in parallel, however, so slow post processing by one destination will
not affect any other destinations.

