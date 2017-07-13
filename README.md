
    They're not staying down there, anymore!

This is a quick-and-dirty IRC bot that listens to HTTP-based
notifications from services like Trello and Github, so you can talk
smack to your Slack-using friends.

Actually, I don't expect it to be used by other people (it certainly
isn't in any state to be reused), but it (almost) never hurts to make
your experiments, toys, and tests public.

Adding Github hooks
-------------------

[See the documentation on Github](https://developer.github.com/webhooks/creating/).

Adding Trello hooks
-------------------

Get your application key from [https://trello.com/1/appKey/generate](https://trello.com/1/appKey/generate).

Visit
```
https://trello.com/1/authorize?name=Chudbot&expiration=never&response_type=token&key=[YOUR KEY HERE]
```
in a browser to get a suitable token and setup `priv/trello.config`.

You can then use the functions in `chudbot_trello` to setup your
webhooks.  This is not particularly usable:

```erlang
> {ok, TrelloConfig} =
    file:consult(filename:join([code:priv_dir(chudbot), "trello.config"])).
> chudbot_trello:start().
> BoardsToModelIds = chudbot_trello:list_boards(TrelloConfig).
> BaseUrl = "http://wherever.example.com:8080".
> ModelId = proplists:get_value(<<"My board">>, BoardsToModelIds).
> chudbot_trello:subscribe_webhook(ModelId, TrelloConfig, BaseUrl).
```


Adding BuildBot hooks
---------------------

Use [HttpStatusPush](http://docs.buildbot.net/0.8.1/HttpStatusPush.html):

```python
import buildbot.status.status_push
sp = buildbot.status.status_push.HttpStatusPush(serverUrl="http://example.com/chudbot/buildbot")
c['status'].append(sp)
```

Beware that buildbot will relentlessly hammer chudbot if it thinks the
status push failed.
