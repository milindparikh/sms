%% @doc Implementation of the KMV DV data structure.
%% @reference [On Synopses for Distinct-Value Estimation Under Multiset Operations.]

-module(kmv).
-export([new/1,  add_element/3, count_elements/1]).
			

-record(kmv, {
    m      = 0,       % The size of the list.
    l = [], 
    f = fun(X, Y) -> {A, _,_,_} = X, {C,_,_,_} = Y, A < C end,
    r=65536
}).


%% @spec new(M) -> kmv()
%% @doc Creates a new kmv, given a maximum number of keys
%%     M is the number of bits required to represent the bloom filter
 
new(M)  ->
    #kmv{m=M}.

%% @spec add_element(string(), bloom()) -> bloom()
%% @doc Adds the key to the filter.
add_element(PKey, Value, #kmv{ m = M, l = L, f=F, r=R} = K)  ->
    Key = erlang:phash2(PKey, R)/R,

    case lists:keyfind(Key, 1, L) of      
         {ActualKey, ActualValue, RealKey, NumberOfFlips} ->  %% Found an existing key in List

           case RealKey =:= PKey of 
             false ->

                K#kmv{l = lists:keyreplace(ActualKey, 1, L, {ActualKey, Value, PKey, NumberOfFlips+1})};
             true -> 

 	         K#kmv{l = lists:keyreplace(ActualKey, 1, L, {ActualKey, (ActualValue + Value), RealKey, NumberOfFlips})}
           end;

         false -> %% DID NOT Found an existing key in List

           case(length(L)) == 0 of 
              true  -> KeyMax = 1; 
              false -> TMax = lists:last(L), {KeyMax, _, _, _ } = TMax
           end,

           case Key < KeyMax of 
               true ->  %% when the candidate is smaller than that max
                 L1 = lists:sort(F, [{Key, Value,PKey,0} | L]),
	         case  (length (L1) > M) of      
                   true ->              %% Lets constraint growth to capacity
                      LF = lists:delete(lists:last(L1), L1),    %by pruning the last on the list
		      K#kmv{l = LF };
                   false ->             %% lets NOT constraint growth
                      LF = L1,
		      K#kmv{l = LF }
                 end;
               false -> 
                 case (length (L) < M) of      
                     true  ->
                       L1 = lists:sort(F, [{Key, Value,PKey,0} | L]),
		       K#kmv{l = L1 };
                     false ->
		         LF = L,
			 K#kmv{l = LF }
                 end
            end
    end.
     

count_elements(#kmv{ m = M, l = L} ) -> 
		     {KMax, _,_,_} =  lists:last(L),
		     (M-1)/KMax.
