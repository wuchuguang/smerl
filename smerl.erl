%%
%% @title Smerl: Simple Metaprogramming for Erlang
%%
%% @doc <p>Smerl is an <a href="http://www.erlang.org">Erlang</a> library
%%   that simplifies the creation and manipulation of Erlang modules in
%%   runtime.</p>
%%   <p>Smerl uses Erlang's capabilities for hot code swapping and
%%   abstract syntax tree parsing to do its magic. Smerl is inspired by
%%   the rdbms_codegen.erl module in the RDBMS application written by
%%   Ulf Wiger. RDBMS is part of <a href="http://jungerl.sf.net">Jungerl</a>.</p>
%%
%%   <p>Here's a quick example illustrating how to use Smerl:</p>
%%   ``
%%   test_smerl() ->
%%     C1 = smerl:new(foo),
%%     {ok, C2} = smerl:add_func(C1, "bar() -> 1 + 1."),
%%     smerl:compile(C2),
%%     foo:bar(),   % returns 2``
%%     smerl:has_func(C2, bar, 0). % returns true
%%
%%   <p>New functions can be expressed either as strings of Erlang code
%%   or as abstract forms. For more information, read the Abstract Format
%%   section in the ERTS User's guide
%%   (<a href="http://erlang.org/doc/doc-5.5/erts-5.5/doc/html/absform.html#4">link</a>).</p>
%%
%%   <p>Using the abstract format, the 3rd line of the above example
%%   would be written as</p>
%%   ``
%%     {ok,C2} = smerl:add_func(C1, {function,1,bar,0,
%%                              [{clause,1,[],[],
%%                               [{op,1,'+',{integer,1,1},{integer,1,1}}]}]).``
%%
%%   <p>The abstact format may look more verbose in this example, but
%%   it's also more amenable to runtime manipulation.</p>
%%
%%   <p>To manipulate or query an existing module rather than a new module,%%
%%     the first line could be rewritten such as:</p>
%%
%%    ``smerl:for_module(mnesia),''
%%
%%   <p><u>Detailed Description</u></p>
%%   <p>With Smerl, you can both create new modules and manipulate existing
%%   modules in runtime. You can also query whether a module has a given
%%   function by calling smerl:has_func. To start creating a new module, call
%%   smerl:new(ModuleName). To start modifying an existing module,
%%   call smerl:for_module(ModuleName). (The module be accessible
%%   with code:which and either have been compiled debug_info or its source
%%   file must in the same directory as the .beam file or in a ../src directory
%%   relative to the .beam file's ./ebin directory.) By calling smerl:for_file,
%%   you can create a new module from an Erlang source file.</p>
%%
%%   <p>smerl:new, smerl:for_module and smerl:for_file return an
%%   opaque context record for the module. To manipulate the module,
%%   use smerl:add_func and smerl:remove_func. Just remember not to
%%   add the same function name with the same arity twice as it will
%%   eventually result in a compilation error.</p>
%%
%%   <p>When you're ready to compile your module, call smerl:compile,
%%   passing in the opaque context record. If there are no errors,
%%   you can start using the new module.</p>
%%
%%   <p>New capabilities (8/16/06):</p>
%%   <p>smerl:get_func, retrieves the abstract form for a given function,
%%   and smerl:replace_func, which does a smerl:remove_func followed by a
%%   smerl:add_func.
%%
%%   <p>New capabilities (8/17/06):</p>
%%   <p>smerl:add_func and smerl:replace_func can now accept fun expressions
%%   as parameters. Now, you longer have to rely on source strings and abstract
%%   forms to add behaviour to a module. Even closures are supported. Closure
%%   variables are expanded in the beginning the function. Example:
%%
%%   ``
%%   A = 15.
%%   C = smerl:new(foo),
%%   {ok, C2} = smerl:add_func(C, bar, fun(B) -> A + B + 37 end),
%%   smerl:compile(C2),
%%   foo:bar(5). % returns 57``
%%
%%   <p>Both smerl:add_func and smerl:replace_func support fun expressions.</p>
%%
%%
%%   
%% @author Yariv Sadan <yarivvv@gmail.com> [http://yarivsblog.com]

%% * Copyright (c) 2006, Yariv Sadan
%% * All rights reserved.
%% * Redistribution and use in source and binary forms, with or without
%% * modification, are permitted provided that the following conditions are met:
%% *
%% *     * Redistributions of source code must retain the above copyright
%% *       notice, this list of conditions and the following disclaimer.
%% *     * Redistributions in binary form must reproduce the above copyright
%% *       notice, this list of conditions and the following disclaimer in the
%% *       documentation and/or other materials provided with the distribution.
%% *     * Neither the name of the Yariv Sadan nor the
%% *       names of its contributors may be used to endorse or promote products
%% *       derived from this software without specific prior written permission.
%% *
%% * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
%% * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% * DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
%% * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(smerl).
-author("Yariv Sadan (yarivvv@gmail.com").
-export([new/1,
	 for_module/1,
	 for_file/1,
         get_module/1,
         get_forms/1,
         get_exports/1,
	 add_func/2,
	 add_func/3,
	 add_func/4,
	 remove_func/3,
	 has_func/3,
	 get_func/3,
	 replace_func/2,
	 replace_func/3,
	 compile/1,
	 rename/2,
	 curry/2,
	 curry/4,
	 curry/5,
	 curry_add/3,
	 curry_add/4,
	 curry_add/6,
	 curry_replace/3,
	 curry_replace/4
	]).

-define(L(Obj), io:format("LOG ~w ~p\n", [?LINE, Obj])).
-define(S(Obj), io:format("LOG ~w ~s\n", [?LINE, Obj])).	 

%% @type meta_ctx(). An opaque record used for manipulating a module.
%% @type func_form(). The abstract form for the function, as described
%%    in the ERTS Users' manual.

%% This is the context record. It's meant for use only internally.
-record(meta_ctx, {module, exports = [], forms = []}).

%% @doc Create a context record for a new module with the given module name.
%%
%% @spec new(Module::atom()) -> meta_ctx()
new(ModuleName) when is_atom(ModuleName) ->
    #meta_ctx{module = ModuleName}.

%% @doc Create a context record for manipulating an existing module.
%%
%% @spec for_module(ModuleName::atom() || string()) ->
%%   {ok, meta_ctx()} | {error, Error}
for_module(ModuleName) when is_list(ModuleName) ->
    new(list_to_atom(ModuleName));
for_module(ModuleName) when is_atom(ModuleName) ->
    case code:which(ModuleName) of
	Path when is_list(Path) ->
	    case get_forms(ModuleName, Path) of
		{ok, Forms} ->
		    ctx_for_forms(Forms);
		_Other ->    
		    {error, invalid_module}
	    end;
	Err ->
	    {error, Err}
    end.

%% @doc Create a context record for a module described in the given source file.
%%
%% @spec for_file(SrcFilePath::string()) -> {ok, meta_ctx()} | {error, invalid_module}
for_file(SrcFilePath) ->
    case epp:parse_file(SrcFilePath, [], []) of
	{ok, Forms} ->
	    ctx_for_forms(Forms);
	_err ->
	    {error, invalid_module}
    end.


get_module(MetaCtx) -> 
    MetaCtx#meta_ctx.module.

get_forms(MetaCtx) ->
    MetaCtx#meta_ctx.forms.

get_exports(MetaCtx) ->
    MetaCtx#meta_ctx.exports.

ctx_for_forms([_FileAttribute, {attribute, _, module, ModuleName}|Forms]) ->
    {Exports, OtherForms} =
	lists:foldl(
	  fun({attribute, _, export, ExportList},
	      {ExportsAcc, OtherAcc}) ->
		  {ExportList ++ ExportsAcc, OtherAcc};
	     ({eof, _}, Acc) ->
		  Acc;
	     (Form, {ExportsAcc, OtherAcc}) ->
		  {ExportsAcc, [Form | OtherAcc]}
	  end, {[], []}, Forms),
    {ok, #meta_ctx{module = ModuleName,
		   exports = Exports,
		   forms = OtherForms
		  }};
ctx_for_forms(_) ->
    {error, invalid_module}.

%% Get the abstract representation, if available, for the module.
%%
%% Strategy:
%% 1) Try to get the abstract code from the module if it's compiled
%%    with debug_info.
%% 2) Look for the source file in the beam file's directory.
%% 3) If the beam file's directory ends with 'ebin', then search in
%%    [beamdir]/../src
get_forms(Module, Path) ->
    case beam_lib:chunks(Path, [abstract_code]) of
	{ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
	    {ok, Forms};
	_Err ->
	    case filename:find_src(Module, [{"ebin", "src"}]) of
		{error, _} = Err ->
		    Err;
		{SrcPath, _} ->
		    Filename = SrcPath ++ ".erl",
		    epp:parse_file(Filename, [], [])
	    end
    end.

%% @doc Add a new function to the module that corresponds to the given context,
%%   and return the new context.
%%   The new function will be added to the module's export list.
%%
%% @spec add_func(MetaCtx::meta_ctx(), Form::func_form() | string()) ->
%%   {ok, NewCtx::meta_ctx()} | {error, parse_error}
add_func(MetaCtx, Form) ->
    add_func(MetaCtx, Form, true).

%% @doc Add a new function to the module that corresponds to the given context,
%%   and return the new context. If Export == false, the function will not
%%   be added to the module's export list.
%%
%% @spec add_func(MetaCtx::meta_ctx(), Func::func_form() | string()) ->
%%   {ok, NewCtx::meta_ctx()} | {error, parse_error}
add_func(MetaCtx, Func, Export) when is_list(Func) ->
    case parse_func_string(Func) of
	{ok, Form} ->
	    add_func(MetaCtx, Form, Export);
	Err ->
	    Err
    end;
add_func(MetaCtx, {function, _Line, FuncName, Arity, _Clauses} = Form,
	 true) ->
    Foo = {ok, MetaCtx#meta_ctx{
      exports = [{FuncName, Arity} | MetaCtx#meta_ctx.exports],
      forms = [Form | MetaCtx#meta_ctx.forms]
     }},
    Foo;
add_func(MetaCtx, {function, _Line, _FuncName, _Arity, _Clauses} = Form,
	 false) ->
   {ok, MetaCtx#meta_ctx{forms = [Form | MetaCtx#meta_ctx.forms]}};

add_func(MetaCtx, Name, Fun) when is_function(Fun) ->
    add_func(MetaCtx, Name, Fun, true);

add_func(_, _, _) ->
    {error, parse_error}.

add_func(MetaCtx, Name, Fun, Export) when is_function(Fun) ->
    case form_for_fun(Name, Fun) of
	{ok, Form} ->
	    add_func(MetaCtx, Form, Export);
	Err ->
	    Err
    end.

form_for_fun(Name, Fun) ->
    Line = 999,
    Info = erlang:fun_info(Fun),
    case Info of
	[{module, _ModName}, _FuncName, _Arity, _Env, {type, external}] ->
	    {error, cant_add_external_funcs};
	[_Pid, _Module, _NewIdx, _NewUniq, _Index, _Uniq, _Name,
	 {arity, Arity},
	 {env, [Vars, _Unknown1, _Unknown2, Clauses]},
	 {type, local}] ->
	    EnvVars = lists:map(
			fun({VarName, Val}) ->
				{match,Line,{var,Line,VarName}, erl_parse:abstract(Val)}
			end, Vars),
	    NewClauses = lists:map(
			   fun({clause, Line1, Params, Guards, Exprs}) ->
				   {clause, Line1, Params, Guards, EnvVars ++ Exprs}
			   end, Clauses),
	    {ok, {function, Line, Name, Arity, NewClauses}};
	_Other ->
	    {error, bad_fun}
    end.


parse_func_string(Func) ->
    case erl_scan:string(Func) of
	{ok, Toks, _} ->
	    case erl_parse:parse_form(Toks) of
		{ok, _Form} = Res ->
		    Res;
		_Err ->
		    {error, parse_error}
	    end;
	_Err ->
	    {error, parse_error}
    end.

%% @doc Try to remove the function from the module represented by the given
%% context record and return the new context record.
%% If the function isn't found, the original context record is returned.
%%
%% @spec remove_func(MetaCtx::meta_ctx(), FuncName::string(), Arity::integer())
%%   -> NewCtx::meta_ctx()
%%
remove_func(MetaCtx, FuncName, Arity) ->
    MetaCtx#meta_ctx{forms =
		     lists:filter(
		       fun({function, _Line, FuncName1, Arity1, _Clauses})
			  when FuncName1 =:= FuncName, Arity =:= Arity1->
			       false;
			  (_) ->
			       true
		       end, MetaCtx#meta_ctx.forms),
		     exports =
		     lists:filter(
		       fun({FuncName1, Arity1})
			  when FuncName1 =:= FuncName,
			       Arity1 =:= Arity ->
			       false;
			  (_) ->
			       true
		       end, MetaCtx#meta_ctx.exports)
		      }.

%% @doc Inspect whether a module has a function with the given name
%%   and arity.
%% @spec has_func(MetaCtx::meta_ctx(), FuncName::atom(), Arity::integer()) ->
%%   bool()
has_func(MetaCtx, FuncName, Arity) ->
    lists:any(fun({function, _Line, FuncName1, Arity1, _Clauses})
		 when FuncName1 == FuncName, Arity1 == Arity ->
		      true;
		 (_) ->
		      false
	      end, MetaCtx#meta_ctx.forms).


%% @doc Get the form for the given function/arity.
%% 
%% @spec get_func(MetaCtx::meta_ctx(), FuncName::atom(), Arity::integer()) ->
%%   {ok, func_form()} | {error, not_found}
get_func(MetaCtx, FuncName, Arity) ->
    get_func2(MetaCtx#meta_ctx.forms, FuncName, Arity).

get_func2([], _FuncName, _Arity) ->
    {error, not_found};
get_func2([{function, _Line, FuncName, Arity, _Clauses} = Form | _Rest], FuncName, Arity) ->
    {ok, Form};
get_func2([_Form|Rest], FuncName, Arity) ->		      
    get_func2(Rest, FuncName, Arity).


%% Replace an existing function with the new one. If the function doesn't exist
%% the new function is simply added to the module. This function basically calls
%% smerl:remove_func followed by smerl:add_func.
%%
%% @spec replace_func(MetaCtx::meta_ctx(), Function::string() | func_form()) ->
%%   {ok, NewCtx::meta_ctx()} | {error, Error}
replace_func(MetaCtx, Function) when is_list(Function) ->
    case parse_func_string(Function) of
	{ok, Form} ->
	    replace_func(MetaCtx, Form);
	Err ->
	    Err
    end;
replace_func(MetaCtx, {function, _Line, FuncName, Arity, _Clauses} = Form) ->
    Ctx1 = remove_func(MetaCtx, FuncName, Arity),
    add_func(Ctx1, Form);
replace_func(_MetaCtx, _) ->
    {error, parse_error}.

%% @doc Simliar to replace_func/2, but accepts a function name + fun expression.
%%
%% @spec replace_func(MetaCtx::meta_ctx(), Name::atom(), Fun::function()) ->
%%   {ok, NewCtx::meta_ctx()} | {error, Error}
replace_func(MetaCtx, Name, Fun) when is_function(Fun) ->
    case form_for_fun(Name, Fun) of
	{ok, Form} ->
	    replace_func(MetaCtx, Form);
	Err ->
	    Err
    end.
	    
    

%% @doc Compile the module represented by the given context record.
%% You should call this function once you're done manipulating your
%% module and you're ready to deploy the changes into the system.
%%
%% @spec compile(MetaCtx::meta_ctx()) -> ok | {error, Error}
compile(MetaCtx) ->
    Forms = [{attribute, 1, module, MetaCtx#meta_ctx.module},
	     {attribute, 2, export, lists:reverse(
				      MetaCtx#meta_ctx.exports)}] ++
	     lists:reverse(MetaCtx#meta_ctx.forms),
    case compile:forms(Forms,
		       [report_errors, report_warnings]) of
	{ok, Module, Bin} ->
	    code:purge(Module),
	    case code:load_binary(Module,
				  atom_to_list(Module) ++ ".erl", Bin) of
		{module, _Module} ->
		    ok;
		Err ->
		    Err
	    end;
	Err ->
	    Err
    end.

%% @doc Change the name of the function represented by the form.
%%
%% @spec rename(Form::func_form(), NewName:atom()) ->
%%   {ok, NewForm::func_form()} | {error, Err}
rename({function, Line, _Name, Arity, Clauses}, NewName) ->
    {function, Line, NewName, Arity, Clauses}.
    
%% @doc Get the curried form for the given function and parameter
%%
%% @spec curry(Form::func_form(), Param::term() | list()) ->
%%   {ok, NewForm::func_form()} | {error, Err}
curry(Form, Param) when not is_list(Param) ->
    curry(Form, [Param]);
curry({function, _Line, _Name, Arity, _Clauses}, Params)
  when length(Params) > Arity ->
    {error, too_many_params};
curry({function, Line, Name, Arity, Clauses}, NewParams) ->
    NewClauses =
	lists:foldl(
	  fun({clause, L1, ExistingParams, _Guards, Exprs},
	      Clauses1) ->
		  {FirstParams, LastParams} =
		      lists:split(length(NewParams), ExistingParams),
		  ZippedParams = lists:zip(FirstParams, NewParams),
		  Matches =
		      lists:foldl(
			fun({Var, NewVal}, Acc) ->
				[{match,1,Var, erl_parse:abstract(NewVal)} | Acc]
			end, [], ZippedParams),
		  [{clause, L1, LastParams, _Guards,
		   Matches ++ Exprs} | Clauses1]
	  end, [], Clauses),
    {ok, {function, Line, Name, Arity-length(NewParams), NewClauses}}.


%% @doc Curry the given function from the given module with
%%  the given param(s)
%%
%% @spec curry(ModName::atom(), Name::atom(), arity::integer(),
%%   Params::term() | list()) ->
%%    {ok, NewForm} | {error, Err}
curry(ModName, Name, Arity, Params) when is_atom(ModName) ->
    case for_module(ModName) of
	{ok, MetaCtx} ->
	    curry(MetaCtx, Name, Arity, Params);
	Err ->
	    Err
    end;

%% @doc Curry the given function from the MetaCtx object with
%%  the given param(s)
%%
%% @spec curry(MetaCtx::meta_ctx(), Name::atom(), arity::integer(),
%%   Params::term() | list()) ->
%%    {ok, NewForm} | {error, Err}
curry(MetaCtx, Name, Arity, Params) ->
    case get_func(MetaCtx, Name, Arity) of
	{ok, Form} ->
	    curry(Form, Params);
	Err ->
	    Err
    end.



%% @doc Curry the given function from the given module or MetaCtx
%%  object with the given param(s), and return its renamed form.
%%
%% @spec curry(Module::atom() | meta_ctx(), Name::atom(), arity::integer(),
%%   Params::term() | list()) ->
%%    {ok, NewForm} | {error, Err}
curry(Module, Name, Arity, Params, NewName) ->
    case curry(Module, Name, Arity, Params) of
	{ok, NewForm} ->
	    {ok, rename(NewForm, NewName)};
	Err ->
	    Err
    end.
		    

%% @doc Add the curried form for the  given function in the
%%   MetaCtx object with its curried form.
%%
%% @spec curry_add(MetaCtx::meta_ctx(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaCtx::meta_ctx()} | {error, Err}
curry_add(MetaCtx, {function, _Line, Name, Arity, _Clauses}, Params) ->
    curry_add(MetaCtx, Name, Arity, Params).

%% @doc Add the curried form for the given function name and arity
%%   in the MetaCtx object with its curried form.
%%
%% @spec curry_add(MetaCtx::meta_ctx(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaCtx::meta_ctx()} | {error, Err}
curry_add(MetaCtx, Name, Arity, Params) ->
    curry_add(MetaCtx, Name, Arity, Params, false).

%% @doc Add the curried form of the function with the given name
%%   and arity in the given module. The function is added with
%%   the new name.
%%
%% @spec curry_add(MetaCtx::meta_ctx(), Module:atom(),
%%   Name::atom(), Arity::integer(), Params::term() | list(),
%%   NewName::atom()) ->
%%     {ok, NewCtx::meta_ctx()} | {error, Error}
curry_add(MetaCtx, Module, Name, Arity, Params, NewName) ->
    case curry(Module, Name, Arity, Params, NewName) of
	{ok, Form} ->
	    add_func(MetaCtx, Form);
	Err ->
	    Err
    end.

curry_add(MetaCtx, Name, Arity, Params, Remove) ->
    case get_func(MetaCtx, Name, Arity) of
        {ok, OldForm} ->
            case curry(OldForm, Params) of
                {ok, NewForm} ->
		    MetaCtx1 =
			case Remove of
			    true ->
				remove_func(MetaCtx, Name, Arity);
			    false ->
				MetaCtx
			end,
		    add_func(MetaCtx1, NewForm);
		Err ->
		    Err
            end;
        Err ->
            Err
    end.

%% @doc Replace the given function in the MetaCtx object with
%%   its curried form.
%%
%% @spec curry_replace(MetaCtx::meta_ctx(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaCtx::meta_ctx()} | {error, Err}
curry_replace(MetaCtx, {function, _Line, Name, Arity, _Clauses}, Params) ->
    curry_replace(MetaCtx, Name, Arity, Params).


%% @doc Replace the given function in the MetaCtx object with
%%   its curried form.
%%
%% @spec curry_replace(MetaCtx::meta_ctx(), name::string(),
%%   Arity::integer(), Params::term() | list()) ->
%%    {ok, NewMetaCtx::meta_ctx()} | {error, Err}
curry_replace(MetaCtx, Name, Arity, Params) ->
    curry_add(MetaCtx, Name, Arity, Params, true).
