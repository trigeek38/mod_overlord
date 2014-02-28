{% extends "overlord_base.tpl" %}

{% block title %}{{ m.site.title }}{% endblock %}

{% block main %}

<div id="slide_content">
</div>

{{ m.rsc.page_home.body|show_media }}
    {% wire action={connect signal={refresh_subscriber_list}
                    action={update target="subscriber-list" template="_subscriber_list.tpl"}
                    action={ update target="subscribe-buttons" template="_subscribe_buttons.tpl" }
                   }
    %}
{% endblock %}


{% block subnavbar %}
    {% include "_subscribe_buttons.tpl" %}
    {% include "_subscriber_list.tpl" %}
    {% include "_collection_list.tpl" %}
{% endblock %}
