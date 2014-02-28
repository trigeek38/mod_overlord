<ul>
{% for c in m.search[{query cat="collection"}] %}
    <li id="{{#collection.c}}" style="cursor: pointer;">{{ c.title }}</li>
        {% wire id=#collection.c action={postback postback= {update_slide target="slide_content" template="_slide_content.tpl" id=c.id } delegate="mod_overlord"} %}
        <ul>
            {% for i in c.o.haspart %}
                <li id="{{#item.i}}" style="cursor: pointer;">{{ i.title }}</li>
                {% wire id=#item.i action={postback postback= {update_slide target="slide_content" template="_slide_content.tpl" id=i.id } delegate="mod_overlord"} %}
            {% endfor %}
        </ul>
{% endfor %}
</ul>
   
