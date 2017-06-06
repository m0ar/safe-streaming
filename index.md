---
# You don't need to edit this file, it's empty on purpose.
# Edit theme's home layout instead if you wanna make some changes
# See: https://jekyllrb.com/docs/themes/#overriding-theme-defaults
layout: default
---

<ul class="posts">
    {% for post in site.posts %}
        <div class="post">
            
            <a href="{{ site.baseurl }}{{ post.url }}" title="{{ post.title }}"> <h2>{{ post.title }}</h2> </a>
            {{ post.excerpt }}
            . . .


        </div>
    {% endfor %}
</ul>
