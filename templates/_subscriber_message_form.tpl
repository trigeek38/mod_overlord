<form id="subscriber_message_form" class="form-inline" action="postback" method="POST">
  <input id="message" name="message" type="text" class="input-xlarge" placeholder="Message">
  <button type="submit" class="btn btn-primary">Post</button>
</form>
{% wire id="subscriber_message_form" type="submit"
                                                   action={dialog_close}
                                                   postback={subscriber_message pid=pid}
                                                   delegate="mod_overlord" %}

