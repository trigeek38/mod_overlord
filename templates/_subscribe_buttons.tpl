<div id="subscribe-buttons">  
 {% if m.overlord.is_subscribed %}
    {% button id="stop-following" class="btn btn-default" text="Stop Following" %}
      {% wire id="stop-following" action={ postback postback={ subscriber_delete }
                                           action={ update target="subscribe-buttons" template="_subscribe_buttons.tpl" }
                                           delegate="mod_overlord" } %}
  {% else %}
    {% button id="start-following" class="btn btn-default" text="Follow" %}
      {% wire id="start-following" action={ postback postback={ subscriber_put }
                                            action={ update target="subscribe-buttons" template="_subscribe_buttons.tpl" }
                                            delegate="mod_overlord" } %}
  {% endif %}
</div>
