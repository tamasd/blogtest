<div class="row">
	<div class="span8">
		<form action="/post" name="post-editor-form" class="well" method="{{ method }}">
			<label>Title</label>
			<input type="hidden" name="id" value="{{ id }}" />
			<input type="text" class="span3" name="title" placeholder="Blogpost title" value="{{ title }}" />
			<label>Post</label>
			<textarea class="input-xlarge" name="body">{{ body }}</textarea>
			<div class="form-actions">
				<button type="submit" class="btn btn-primary">Save</button>
				{% if id %}
					<a class="btn" href="/post/{{ id }}">Cancel</a>
				{% else %}
					<a class="btn" href="/">Cancel</a>
				{% endif %}
			</div>
		</form>
	</div>
</div>