<div id="subscriber-list">
    {% for subscriber in m.overlord.subscribers %}
        <div>
            {% if m.rsc[subscriber.user].title|is_undefined %}
                Guest
            {% else %}
                {{ m.rsc[subscriber.user].title }}
            {% endif %}
                <span class="actions">
                   <a id="delete-subscriber-{{subscriber.user}}-{{forloop.counter}}"  href="#"><i class="icon-trash"></i> Remove</a>
                   <span class="pull-right"><a id="message-subscriber-{{subscriber.user}}-{{forloop.counter}}" href="#"><i class="icon-comment-alt"></i>Message</a></span>
                </span>
         </div>
            {% wire id=["delete-subscriber",subscriber.user, forloop.counter]|join:"-"
               action={postback postback={subscriber_delete pid=subscriber.pid userid=subscriber.user} delegate="mod_overlord"
               action={ update target="subscribe-buttons" template="_subscribe_buttons.tpl" }
               action={remove target=["subscriber",subscriber.user,forloop.counter]|join:"-"}} %}

            {% wire id=["message-subscriber", subscriber.user, forloop.counter]|join:"-"
               action={dialog_open template="_subscriber_message_form.tpl" title="Send Message" pid=subscriber.pid} %}
    {% endfor %}
</div>
