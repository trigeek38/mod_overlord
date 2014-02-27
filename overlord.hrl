%% Record definition for mod_overlord

-record(subscriber_get, {userid}).
-record(subscriber_put, {pid, userid}).
-record(subscriber_delete, {pid, userid}).
-record(subscriber_delete_all, {userid}).
