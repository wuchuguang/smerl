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
%%     M1 = smerl:new(foo),
%%     {ok, M2} = smerl:add_func(M1, "bar() -> 1 + 1."),
%%     smerl:compile(M2),
%%     foo:bar(),   % returns 2``
%%     smerl:has_func(M2, bar, 0). % returns true
%%
%%   <p>New functions can be expressed either as strings of Erlang code
%%   or as abstract forms. For more information, read the Abstract Format
%%   section in the ERTS User's guide
%%   (<a href="http://erlang.org/doc/doc-5.5/erts-5.5/doc/html/absform.html#4">link</a>).</p>
%%
%%   <p>Using the abstract format, the 3rd line of the above example
%%   would be written as</p>
%%   ``
%%     {ok,M2} = smerl:add_func(M1, {function,1,bar,0,
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
%%   MetaMod object for the module. To manipulate the module,
%%   use smerl:add_func and smerl:remove_func. Just remember not to
%%   add the same function name with the same arity twice as it will
%%   eventually result in a compilation error.</p>
%%
%%   <p>When you're ready to compile your module, call smerl:compile,
%%   passing in the MetaMod object. If there are no errors,
%%   you can start using the new module.</p>
%%
%%   <p>New capabilities (8/16/06):</p>
%%   <p>smerl:get_func, retrieves the abstract form for a given function,
%%   and smerl:replace_func, which does a smerl:remove_func followed by a
%%   smerl:add_func.
%%
%%   <p>New capabilities (8/17/06):</p>
%%   <p>smerl:add_func and smerl:replace_func can now accept fun expressions
%%   as parameters. <b>This only works in the Erlang shell at the moment</b>.
%%   With fun expressions, you longer have to rely on source strings and abstract
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
	 set_module/2,
         get_forms/1,
	 set_forms/2,
         get_exports/1,
	 set_exports/2,
	 remove_export/3,
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
	 curry_add/5,
	 curry_add/6,
	 curry_replace/3,
	 curry_replace/4,
	 embed_params/2,
	 embed_all/2,
	 extend/2
	]).

-define(L(Obj), io:format("LOG ~w ~p\n", [?LINE, Obj])).
-define(S(Obj), io:format("LOG ~w ~s\n", [?LINE, Obj])).	 

%% @type meta_mod(). A tuple holding the abstract representation for module.
%% @type func_form(). The abstract form for the function, as described
%%    in the ERTS Users' manual.

%% The record type holding the abstract representation for a module.
-record(meta_mod, {module, exports = [], forms = []}).

%% @doc Create a record for a new module with the given module name.
%%
%% @spec new(Module::atom()) -> meta_mod()
new(ModuleName) when is_atom(ModuleName) ->
    #meta_mod{module = ModuleName}.

%% @doc Create a MetaMod object for manipulating an existing module.
%%
%% @spec for_module(ModuleName::atom() || string()) ->
%%   {ok, meta_mod()} | {error, Error}
for_module(ModuleName) when is_list(ModuleName) ->
    new(list_to_atom(ModuleName));
for_module(ModuleName) when is_atom(ModuleName) ->
    case code:which(ModuleName) of
	Path when is_list(Path) ->
	    case get_forms(ModuleName, Path) of
		{ok, Forms} ->
		    mod_for_forms(Forms);
		_Other ->    
		    {error, invalid_module}
	    end;
	Err ->
	    {error, Err}
    end.

%% @doc Create a MetaMod object for a module described in the given source
%% file.
%%
%% @spec for_file(SrcFilePath::string()) -> {ok, meta_mod()} |
%%   {error, invalid_module}
for_file(SrcFilePath) ->
    case epp:parse_file(SrcFilePath, [], []) of
	{ok, Forms} ->
	    mod_for_forms(Forms);
	_err ->
	    {error, invalid_module}
    end.


%% @doc Return the module name for the MetaMod object.
%%
%% @spec(MetaMod::meta_mod()) -> atom()
get_module(MetaMod) -> 
    MetaMod#meta_mod.module.

%% @doc Set the MetaMod object's module name to the new name.
%%
%% @spec set_module(MetaMod::meta_mod(), NewName::atom()) ->
%%   NewMod::meta_mod()
set_module(MetaMod, NewName) ->
    MetaMod#meta_mod{module = NewName}.

%% @doc Return the list of function forms in the MetaMod object.
%%
%% @spec get_forms(MetaMod::meta_mod()) -> list()
get_forms(MetaMod) ->
    MetaMod#meta_mod.forms.

set_forms(MetaMod, Forms) ->
    MetaMod#meta_mod{forms = Forms}.

%% @doc Return the list of exports in the MetaMod object.
%%
%% @spec get_exports(MetaMod::meta_mod()) -> list()
get_exports(MetaMod) ->
    MetaMod#meta_mod.exports.

set_exports(MetaMod, Exports) ->
    MetaMod#meta_mod{exports = Exports}.

%% @doc Remove the given export from the list of exports in the MetaMod object.
%%
%% @spec remove_export(MetaMod::meta_mod(), FuncName::atom(),
%%   Arity::integer()) -> NewMod::meta_mod()
remove_export(MetaMod, FuncName, Arity) ->
    MetaMod#meta_mod{exports =
		     lists:delete({FuncName, Arity},
			     MetaMod#meta_mod.exports)}.

mod_for_forms([_FileAttribute, {attribute, _, module, ModuleName}|Forms]) ->
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
    {ok, #meta_mod{module = ModuleName,
		   exports = Exports,
		   forms = OtherForms
		  }};
mod_for_forms(_) ->
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

%% @doc Add a new function to the MetaMod object and return the new MetaMod
%%   object. The new function will be added to the module's export list.
%%
%% @spec add_func(MetaMod::meta_mod(), Form::func_form() | string()) ->
%%   {ok, NewMod::meta_mod()} | {error, parse_error}
add_func(MetaMod, Form) ->
    add_func(MetaMod, Form, true).

%% @doc Add a new function to the MetaMod object and return the new MetaMod
%%   object. If Export == false, the function will not be added to the
%%   MetaMod object's export list.
%%
%% @spec add_func(MetaMod::meta_mod(), Func::func_form() | string()) ->
%%   {ok, NewMod::meta_mod()} | {error, parse_error}
add_func(MetaMod, Func, Export) when is_list(Func) ->
    case parse_func_string(Func) of
	{ok, Form} ->
	    add_func(MetaMod, Form, Export);
	Err ->
	    Err
    end;
add_func(MetaMod, {function, _Line, FuncName, Arity, _Clauses} = Form,
	 true) ->
    Foo = {ok, MetaMod#meta_mod{
      exports = [{FuncName, Arity} | MetaMod#meta_mod.exports],
      forms = [Form | MetaMod#meta_mod.forms]
     }},
    Foo;
add_func(MetaMod, {function, _Line, _FuncName, _Arity, _Clauses} = Form,
	 false) ->
   {ok, MetaMod#meta_mod{forms = [Form | MetaMod#meta_mod.forms]}};

add_func(MetaMod, Name, Fun) when is_function(Fun) ->
    add_func(MetaMod, Name, Fun, true);

add_func(_, _, _) ->
    {error, parse_error}.

add_func(MetaMod, Name, Fun, Export) when is_function(Fun) ->
    case form_for_fun(Name, Fun) of
	{ok, Form} ->
	    add_func(MetaMod, Form, Export);
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

%% @doc Try to remove the function from the MetaMod object return the
%%   resulting MetaMod object.
%%   If the function isn't found, the original MetaMod object is returned.
%%
%% @spec remove_func(MetaMod::meta_mod(), FuncName::string(), Arity::integer())
%%   -> NewMod::meta_mod()
%%
remove_func(MetaMod, FuncName, Arity) ->
    MetaMod#meta_mod{forms =
		     lists:filter(
		       fun({function, _Line, FuncName1, Arity1, _Clauses})
			  when FuncName1 =:= FuncName, Arity =:= Arity1->
			       false;
			  (_) ->
			       true
		       end, MetaMod#meta_mod.forms),
		     exports =
		     lists:filter(
		       fun({FuncName1, Arity1})
			  when FuncName1 =:= FuncName,
			       Arity1 =:= Arity ->
			       false;
			  (_) ->
			       true
		       end, MetaMod#meta_mod.exports)
		      }.

%% @doc Inspect whether a MetaMod object has a function with the given name
%%   and arity.
%% @spec has_func(MetaMod::meta_mod(), FuncName::atom(), Arity::integer()) ->
%%   bool()
has_func(MetaMod, FuncName, Arity) ->
    lists:any(fun({function, _Line, FuncName1, Arity1, _Clauses})
		 when FuncName1 == FuncName, Arity1 == Arity ->
		      true;
		 (_) ->
		      false
	      end, MetaMod#meta_mod.forms).


%% @doc Get the form for the function with the given arity in the
%%   MetaMod object.
%% 
%% @spec get_func(MetaMod::meta_mod() | Module::atom(),
%%   FuncName::atom(), Arity::integer()) ->
%%     {ok, func_form()} | {error, function_not_found}
get_func(Module, FuncName, Arity) when is_atom(Module) ->
    case smerl:for_module(Module) of
	{ok, C1} ->
	    get_func(C1, FuncName, Arity);
	Err ->
	    Err
    end;
get_func(MetaMod, FuncName, Arity) ->
    get_func2(MetaMod#meta_mod.forms, FuncName, Arity).

get_func2([], _FuncName, _Arity) ->
    {error, function_not_found};
get_func2([{function, _Line, FuncName, Arity, _Clauses} = Form | _Rest], FuncName, Arity) ->
    {ok, Form};
get_func2([_Form|Rest], FuncName, Arity) ->		      
    get_func2(Rest, FuncName, Arity).



%% Replace an existing function with the new one. If the function doesn't exist
%% the new function is added to the MetaMod object.
%% This function calls smerl:remove_func followed by smerl:add_func.
%%
%% @spec replace_func(MetaMod::meta_mod(), Function::string() | func_form()) ->
%%   {ok, NewMod::meta_mod()} | {error, Error}
replace_func(MetaMod, Function) when is_list(Function) ->
    case parse_func_string(Function) of
	{ok, Form} ->
	    replace_func(MetaMod, Form);
	Err ->
	    Err
    end;
replace_func(MetaMod, {function, _Line, FuncName, Arity, _Clauses} = Form) ->
    Mod1 = remove_func(MetaMod, FuncName, Arity),
    add_func(Mod1, Form);
replace_func(_MetaMod, _) ->
    {error, parse_error}.

%% @doc Simliar to replace_func/2, but accepts a function
%%   name + fun expression.
%%
%% @spec replace_func(MetaMod::meta_mod(), Name::atom(), Fun::function()) ->
%%   {ok, NewMod::meta_mod()} | {error, Error}
replace_func(MetaMod, Name, Fun) when is_function(Fun) ->
    case form_for_fun(Name, Fun) of
	{ok, Form} ->
	    replace_func(MetaMod, Form);
	Err ->
	    Err
    end.
	    
    

%% @doc Compile the module represented by the MetaMod object.
%% You should call this function once you're done manipulating your
%% module and you're ready to deploy the changes into the VM in runtime.
%%
%% @spec compile(MetaMod::meta_mod()) -> ok | {error, Error}
compile(MetaMod) ->
    Forms = [{attribute, 1, module, MetaMod#meta_mod.module},
	     {attribute, 2, export, lists:reverse(
				      MetaMod#meta_mod.exports)}] ++
	     lists:reverse(MetaMod#meta_mod.forms),
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
	  fun(Clause, Clauses1) ->
		  [curry_clause(Clause, NewParams) | Clauses1]
	  end, [], Clauses),
    {ok, {function, Line, Name, Arity-length(NewParams), NewClauses}}.

curry_clause({clause, L1, ExistingParams, Guards, Exprs}, NewParams) ->
    {FirstParams, LastParams} =
	lists:split(length(NewParams), ExistingParams),
    Matches =
	lists:foldl(
	  fun({Var, NewVal}, Acc) ->
		  [{match, 1, Var, erl_parse:abstract(NewVal)} | Acc]
	  end, [], lists:zip(FirstParams, NewParams)),
    {clause, L1, LastParams, Guards, Matches ++ Exprs}.


%% @doc Curry the given function from the given module with
%%  the given param(s)
%%
%% @spec curry(ModName::atom(), Name::atom(), arity::integer(),
%%   Params::term() | list()) ->
%%    {ok, NewForm} | {error, Err}
curry(ModName, Name, Arity, Params) when is_atom(ModName) ->
    case for_module(ModName) of
	{ok, MetaMod} ->
	    curry(MetaMod, Name, Arity, Params);
	Err ->
	    Err
    end;

%% @doc Curry the given function from the MetaMod object with
%%  the given param(s)
%%
%% @spec curry(MetaMod::meta_mod(), Name::atom(), arity::integer(),
%%   Params::term() | list()) ->
%%    {ok, NewForm} | {error, Err}
curry(MetaMod, Name, Arity, Params) ->
    case get_func(MetaMod, Name, Arity) of
	{ok, Form} ->
	    curry(Form, Params);
	Err ->
	    Err
    end.



%% @doc Curry the given function from the given module or MetaMod
%%  object with the given param(s), and return its renamed form.
%%
%% @spec curry(Module::atom() | meta_mod(), Name::atom(), arity::integer(),
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
%%   MetaMod object with its curried form.
%%
%% @spec curry_add(MetaMod::meta_mod(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaMod::meta_mod()} | {error, Err}
curry_add(MetaMod, {function, _Line, Name, Arity, _Clauses}, Params) ->
    curry_add(MetaMod, Name, Arity, Params).

%% @doc Add the curried form for the given function name and arity
%%   in the MetaMod object with its curried form.
%%
%% @spec curry_add(MetaMod::meta_mod(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaMod::meta_mod()} | {error, Err}
curry_add(MetaMod, Name, Arity, Params) ->
    curry_change(MetaMod, Name, Arity, Params, false).

%% @doc Add the curried form for the function from the given module
%%   to the MetaMod object.
%%
%% @spec curry_add(MetaMod::meta_mod(), Module::atom(), Name::atom(),
%%   Arity::integer(), Params::term() | list()) ->
%%     {ok, NewMod::meta_mod()} | {error, Err}
curry_add(MetaMod, Module, Name, Arity, Params) ->
    case curry(Module, Name, Arity, Params) of
	{ok, Form} ->
	    add_func(MetaMod, Form);	    
	Err ->
	    Err
    end.

%% @doc Curry the function with the given name
%%   and arity in the given module, rename the curried form, and
%%   add it to the MetaMod object.
%%
%% @spec curry_add(MetaMod::meta_mod(), Module:atom(),
%%   Name::atom(), Arity::integer(), Params::term() | list(),
%%   NewName::atom()) ->
%%     {ok, NewMod::meta_mod()} | {error, Error}
curry_add(MetaMod, Module, Name, Arity, Params, NewName) ->
    case curry(Module, Name, Arity, Params, NewName) of
	{ok, Form} ->
	    add_func(MetaMod, Form);
	Err ->
	    Err
    end.

curry_change(MetaMod, Name, Arity, Params, Remove) ->
    case get_func(MetaMod, Name, Arity) of
        {ok, OldForm} ->
            case curry(OldForm, Params) of
                {ok, NewForm} ->
		    MetaMod1 =
			case Remove of
			    true ->
				remove_func(MetaMod, Name, Arity);
			    false ->
				MetaMod
			end,
		    add_func(MetaMod1, NewForm);
		Err ->
		    Err
            end;
        Err ->
            Err
    end.

%% @doc Replace the given function in the MetaMod object with
%%   its curried form.
%%
%% @spec curry_replace(MetaMod::meta_mod(), Form::func_form(),
%%   Params::term() | list()) ->
%%    {ok, NewMetaMod::meta_mod()} | {error, Err}
curry_replace(MetaMod, {function, _Line, Name, Arity, _Clauses}, Params) ->
    curry_replace(MetaMod, Name, Arity, Params).


%% @doc Replace the given function in the MetaMod object with
%%   its curried form.
%%
%% @spec curry_replace(MetaMod::meta_mod(), name::string(),
%%   Arity::integer(), Params::term() | list()) ->
%%    {ok, NewMetaMod::meta_mod()} | {error, Err}
curry_replace(MetaMod, Name, Arity, Params) ->
    curry_change(MetaMod, Name, Arity, Params, true).


%% @doc Embed all parameters that match a Name element of the list of
%%  {Name, Value} pairs in the body of the function by setting them
%%  to the Value element.
%%
%% @spec embed_params(Func::func_form(),
%%   Vals::[{Name::atom(), Value:term()}]) -> NewForm::func_form()}
embed_params({function, L, Name, Arity, Clauses}, Vals) ->
    NewClauses =
	lists:foldl(
	  fun({clause, L1, Params, Guards, Exprs}, Clauses1) ->
		  {Params1, Matches1, _RemainingVals} =
		      lists:foldl(
			fun({var, _L2, ParamName} = Param,
			    {Params2, Matches2, Vals1}) ->
				case lists:keysearch(ParamName, 1, Vals1) of 
				    {value, {_Name, Val} = Elem} ->
					Match = {match, L1, Param,
						 erl_parse:abstract(Val)},
					{Params2, [Match | Matches2],
					 lists:delete(Elem, Vals1)};
				    false ->
					{[Param | Params2], Matches2, Vals1}
				end;
			   (Param, {Params2, Matches2, Vals1}) ->
				{[Param | Params2], Matches2, Vals1}
			end, {[], [], Vals}, Params),
		  [{clause, L1, lists:reverse(Params1), Guards,
				lists:reverse(Matches1) ++ Exprs} | Clauses1]
	  end, [], Clauses),
    NewArity =
	case NewClauses of
	    [{clause, _L2, Params, _Guards, _Exprs}|_] ->
		length(Params);
	    _ ->
		Arity
	end,
    {function, L, Name, NewArity, lists:reverse(NewClauses)}.


%% Apply the embed_params function with the given list of {Name, Value}
%% pairs to all forms in the MetaMod object. Exports are preserved even
%% for functions whose arity has changed.
%%
%% @spec embed_all(MetaMod::meta_mod(), Vals::[{Name::atom(),
%%   Value::term()}]) -> NewMetaMod::meta_mod()
embed_all(MetaMod, Vals) ->
    Forms = get_forms(MetaMod),
    Exports = get_exports(MetaMod),
    {NewForms, Exports3, NewExports} =
	lists:foldl(
	  fun({function, _L, Name, Arity, _Clauses} = Form,
	      {Forms1, Exports1, NewExports1}) ->
		  {function, _, _, NewArity, _} = NewForm =
		      embed_params(Form, Vals),
		  Exports2 = lists:delete({Name, Arity}, Exports1),
		  NewExports2 =
		      case length(Exports2) == length(Exports1) of
			  true ->
			      NewExports1;
			  false ->
			      [{Name, NewArity} | NewExports1]
		      end,
		  {[NewForm | Forms1], Exports2, NewExports2} 
	  end, {[], Exports, []}, Forms),
    {NewForms1, NewExports1} =
	{NewForms, NewExports},
    #meta_mod{module = get_module(MetaMod),
	      exports = Exports3 ++ NewExports1,
	      forms = NewForms1}.



%% @doc extend/2 allows you to create child modules from parent modules.
%% All exported functions that are unique to the parent module are
%% added to the child module in the form of remote function calls
%% to the parent module.
%%
%% @spec extend(Base::atom() | meta_mod(), Child:atom() | meta_mod()) ->
%%   NewChildMod::meta_mod()
extend(Base, Child) when is_atom(Base)->
    case for_module(Base) of
	{ok, MetaMod} ->
	    extend(MetaMod, Child);
	Err ->
	    Err
    end;
extend(ParentMod, Child) when is_atom(Child) ->
    case for_module(Child) of
	{ok, ChildMod} ->
	    extend(ParentMod, ChildMod);
	Err ->
	    Err
    end;
extend(ParentMod, ChildMod) ->
    ParentExports = get_exports(ParentMod),
    ChildExports = get_exports(ChildMod),
    ExportsDiff = ParentExports -- ChildExports,
    lists:foldl(
      fun({FuncName, Arity}, ChildMod1) ->
	      List = lists:seq(1, Arity),
	      Params =
		  lists:foldl(
		    fun(Num, Acc) ->
			    [{var,1,
			      list_to_atom("P" ++
					   integer_to_list(
					     Num + 1))} | Acc]
		    end, [], List),
	      Func =
		  {function,1,FuncName,Arity,
		   [{clause,1,Params,[],
		     [{call,1,
		       {remote,1,{atom,1,get_module(ParentMod)},
			{atom,1,FuncName}},
		       Params}]}
		   ]},
	      {ok, ChildMod2} = add_func(ChildMod1, Func),
	      ChildMod2
      end, ChildMod, ExportsDiff).
