-module(blogtest_resource).

-import(lists, [foreach/2]).

-export([init/1, to_html/2, to_json/2, content_types_provided/2, content_types_accepted/2, allowed_methods/2, resource_exists/2, allow_missing_post/2, post_is_create/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).

-record(blogpost, {id, title, body, date}).

-record(context, {root, response_body=undefined, metadata=[], post=undefined}).

first_run() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(blogpost, [
		{attributes, record_info(fields, blogpost)},
		{disc_copies, [node()]},
		{storage_properties, [
			{ets, [compressed]},
			{dets, [{auto_save, 1000}]}
			]}
		]),
	mnesia:stop().

init(ConfigProps) ->
	{root, Root} = proplists:lookup(root, ConfigProps),
	erlydtl:compile(filename:join([Root, "templates/index.tpl"]), page_template),
	erlydtl:compile(filename:join([Root, "templates/post.tpl"]), post_template),
	erlydtl:compile(filename:join([Root, "templates/edit.tpl"]), edit_template),
	erlydtl:compile(filename:join([Root, "templates/confirm.tpl"]), confirm_template),
	{ok, #context{root=Root}}.
	%{{trace, "/tmp"}, #context{root=Root}}.

allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

post_is_create(ReqData, Context) ->
	Res = case wrq:path_tokens(ReqData) of
		["post", _] -> false;
		["delete", _] -> true;
		_ -> true
	end,
	{Res, ReqData, Context}.

allowed_methods(ReqData, Context) ->
	Methods = case wrq:path_tokens(ReqData) of
		["post", _] -> ['GET', 'POST', 'PUT', 'DELETE'];
		["post"] -> ['POST', 'PUT'];
		["delete", _] -> ['GET', 'POST'];
		_ -> ['GET']
	end,
	{Methods, ReqData, Context}.

create_path(ReqData, Context) ->
	Path = case wrq:path_tokens(ReqData) of
		["post", ID] -> "/post/" ++ ID;
		["post"] -> "/post/" ++ erlang:integer_to_list(get_highest_blogpost_id());
		["delete", _] -> "/"
	end,
	{Path, ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}, {"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	CT = "application/x-www-form-urlencoded",
	CB = case wrq:path_tokens(ReqData) of
		["post", _] -> blogpost_save_page;
		["post"] -> blogpost_save_page;
		["delete", _] -> blogpost_delete_page
	end,
	{[{CT, CB}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

to_html(ReqData, State) ->
	{ok, Content} = case wrq:path_tokens(ReqData) of
		["new"] -> edit_template:render([
				{id, ""},
				{title, ""},
				{body, ""},
				{method, "POST"}]);
		["edit", ID] -> case get_blogpost(ID) of
				false -> {ok, ""};
				P -> edit_template:render([
					{id, P#blogpost.id},
					{title, P#blogpost.title},
					{body, P#blogpost.body},
					{method, "POST"}
				])
			end;
		["post", ID] -> case get_blogpost(ID) of
				false -> {ok, ""};
				P -> post_template:render(blogpost_to_proplist(P))
			end;
		["delete", ID] -> case get_blogpost(ID) of
				false -> {ok, ""};
				P -> confirm_template:render(blogpost_to_proplist(P))
			end;
		_ -> case lists:concat(lists:map(fun(P) -> {ok, C} = post_template:render(blogpost_to_proplist(P)), C end, get_all_blogposts())) of
				[] -> {ok, ""};
				Res -> {ok, Res}
			end
	end,
	{ok, Page} = page_template:render([{content, Content}]),
    {Page, ReqData, State}.

blogpost_save_page(ReqData, State) ->
	Postdata = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
	ID = case proplists:lookup("id", Postdata) of 
		{"id", Id} -> case string:to_integer(Id) of
			{Int, _} when is_integer(Int) -> Int;
			_ -> undefined
		end;
		none -> undefined
	end,
	{"title", Title} = proplists:lookup("title", Postdata),
	{"body", Body} = proplists:lookup("body", Postdata),
	SaveID = case ID of
		I when is_integer(I) -> save_blogpost(ID, Title, Body);
		undefined -> save_blogpost(undefined, Title, Body)
	end,
	LOC = "/post/" ++ erlang:integer_to_list(SaveID),
	{true, wrq:do_redirect(true, wrq:set_resp_header("Location", LOC, ReqData)), State}.

blogpost_delete_page(ReqData, State) ->
	["delete", ID] = wrq:path_tokens(ReqData),
	IntID = case string:to_integer(ID) of
		{Int, _} when is_integer(Int) -> Int
	end,
	exec(fun() -> mnesia:delete({blogpost, IntID}) end),
	{true, wrq:do_redirect(true, wrq:set_resp_header("Location", "/", ReqData)), State}.

resource_exists(ReqData, Context) ->
	Res = case wrq:path_tokens(ReqData) of
		["post", ID] -> case get_blogpost(ID) of
			false -> {false, ReqData, Context};
			BP -> {true, ReqData, Context#context{post=BP}}
		end;
		["post"] -> {true, ReqData, Context};
		["edit", ID] -> case get_blogpost(ID) of
			false -> {false, ReqData, Context};
			BP -> {true, ReqData, Context#context{post=BP}}
		end;
		["new"] -> {true, ReqData, Context};
		["delete", ID] -> case get_blogpost(ID) of
			false -> {false, ReqData, Context};
			BP -> {true, ReqData, Context#context{post=BP}}
		end;
		[] -> {true, ReqData, Context};
		_ -> {false, ReqData, Context}
	end,
	Res.

to_json(ReqData, State) ->
	{"{}", ReqData, State}.

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	exec(F).
exec(F) ->
	case mnesia:transaction(F) of
		{atomic, Val} -> Val;
		{aborted, {node_not_running, _}} -> 
			mnesia:start(),
			mnesia:wait_for_tables([blogpost], 30000),
			exec(F);
		{aborted, {no_exists, _}} ->
			mnesia:stop(),
			first_run(),
			exec(F)
		end.

get_blogpost(ID) ->
	case string:to_integer(ID) of
		{error, _} -> false;
		{IntID, _} ->
			Q = do(qlc:q([X || X <- mnesia:table(blogpost), X#blogpost.id =:= IntID])),
			case Q of
				[] -> false;
			 	R -> hd(R)
			end
	end.

get_highest_blogpost_id() ->
	IDs = qlc:q([X#blogpost.id || X <- mnesia:table(blogpost)]),
	IDsorted = do(qlc:sort(IDs, {order, descending})),
	ID = case IDsorted of
		[] -> 0;
		_ -> hd(IDsorted)
	end,
	ID + 1.

get_all_blogposts() ->
	Q1 = qlc:q([X || X <- mnesia:table(blogpost)]),
	do(qlc:sort(Q1, {order, fun(B1, B2) -> B1#blogpost.date > B2#blogpost.date end})).

save_blogpost(ID, Title, Body) ->
	{MS, S, _} = os:timestamp(),
	SaveID = case ID of
		undefined -> get_highest_blogpost_id();
		I -> I
	end,
	Row = #blogpost{id=SaveID, title=Title, body=Body, date=(MS*1000000+S)},
	F = fun() -> 
		mnesia:write(blogpost, Row, write)
	end,
	exec(F),
	SaveID.

blogpost_to_proplist(Post) ->
	[{id, Post#blogpost.id}, {title, Post#blogpost.title}, {body, Post#blogpost.body}, {date, Post#blogpost.date}].

