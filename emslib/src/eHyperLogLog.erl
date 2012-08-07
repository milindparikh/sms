%% @doc Implementation of the Hyper Log Log Datastructure
%% @reference [http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf]
%% Created: Jul 2, 2012
%% Description: TODO: Add description to HyperLogLog
%% TODO: 
%%  -> Implement All Estimates
%%  -> Write Unit Test Cases
%%  -> Fine tune message passing and remove extra message passing
%%  -> Fix ability to reset registers to 0 without recreating them 
-module(eHyperLogLog).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).
%%
%% API Functions
%%

initialize(RSD) -> initialize(RSD, "").

initialize(RSD, Name) -> initialize(RSD, createRegisters, Name).

initialize(RSD, Type, Name) -> Log2M = log2M(RSD),
				   RegisterCount = trunc(math:pow(2, Log2M)),
				   case Type of 
			  		  createRegisters -> 
				  		destroyExistingRegisters(RegisterCount);
					  _ -> ok
		  		   end,
				   AlphaMM = getAlphaMM(Log2M, RegisterCount),
				   initialize_registers(RegisterCount, Type, Name),
				   spawn(eHyperLogLog, initialize, [Log2M, RegisterCount, AlphaMM, Name]).			   

initialize(Log2M, RegisterCount, AlphaMM, Name) ->
	              receive
					    {Parent, addElement, Element} -> Parent ! add_element(Element, Log2M, Name),
									initialize(Log2M, RegisterCount, AlphaMM, Name);
					    {Parent, getCardinality} -> Parent ! cardinality(RegisterCount, AlphaMM, Name),
				  					initialize(Log2M, RegisterCount, AlphaMM, Name)
                   end.


%%
%% Local Functions
%%

initialize_registers(0, Type, Name) -> io:format("~n Registers Initialized via ~p", [Type]);

initialize_registers(RegisterCount, Type, Name)
     when RegisterCount > 0 ->
		  RegisterName = list_to_atom(string:concat(Name ,integer_to_list(RegisterCount))),	
	      %%RegisterName = list_to_atom(integer_to_list(RegisterCount)),
		  setRegister( RegisterName, Type),
		  initialize_registers(RegisterCount - 1, Type, Name).
    

add_element(Element, Log2M, Name) ->
	BinHash = erlang:phash2(Element, 4294967296),
	<<Idx:Log2M, Rest/bits>> = <<BinHash:32>>,
	Rank = computeRank(Idx),
	Result = pushElementToRegister(Rank, Rest, Name),
	{ok, elementAdded, Result}.

%% Get Log2M for RSD
log2M(RSD) -> trunc(math:log((1.106 / RSD)*(1.106/ RSD))/ math:log(2)). 

%% Get AlphaMM 
getAlphaMM(Log2M, M) ->
     case Log2M of 
       4 -> (0.673 * M * M);
       5 -> (0.697 * M * M);
       6 -> (0.709 * M * M);
       _ -> (0.7213 / (1 + 1.079 / M)) * M * M
     end.

%% Get Rank
computeRank(Idx) -> Idx + 1.

setRegister(RegisterName, Type) ->
	case Type of
		createRegisters -> 
			createRegister(whereis(RegisterName),RegisterName) ;
		resetRegisters ->	resetRegister(RegisterName);
		_ -> io:format("Invalide Type ~p ~n", [Type])
    end.

resetRegister(RegisterName) ->
		 RegisterName ! {self(), initialize},
		 receive Value ->
					io:format("~n ~p Register Value Reset", [Value])	 
		 end.

createRegister(undefined, RegisterName)  ->
	      register(RegisterName, spawn(memoryregister, calculation, [0])),
		  io:format("~n ~p New Memory Register Created", [RegisterName]);
createRegister(_, RegisterName) ->
				    io:format("~n ~p Memory Register cannot be recreated", [RegisterName]).

destroyExistingRegisters(0) -> ok;
destroyExistingRegisters(RegisterCount) ->
	RegisterName = list_to_atom(integer_to_list(RegisterCount)), 
	case whereis(RegisterName) of 
		 undefined -> ok;
		 _ -> exit(whereis(RegisterName), kill) 
	end,
	destroyExistingRegisters(RegisterCount -1).
	
cardinality(RegisterCount, AlphaMM, Name) -> 
							  RegisterSum = getRegisterSum(0 , RegisterCount, Name),
 							  Estimate = getEstimate(RegisterSum, AlphaMM),
 							  RevisedEstimate = getRevisedSmallRangeEstimate(RegisterCount, Name),
 							  RevisedEstimate.

getRegisterSum(RegisterSum, 0, Name) -> RegisterSum;

getRegisterSum(RegisterSum, RegisterCount, Name) -> 
							  TempRegisterSum = RegisterSum + math:pow(2, getRegisterValue(RegisterCount, Name)),
							  getRegisterSum(TempRegisterSum, (RegisterCount-1), Name).

getEstimate(RegisterSum, AlphaMM) -> AlphaMM * (1/ RegisterSum).

getRevisedSmallRangeEstimate(RegisterCount, Name) -> Zeros = getZeros(0, RegisterCount, Name),
											   trunc(RegisterCount * math:log( RegisterCount/ Zeros)).

getZeros(ZeroCount, 0, Name) -> ZeroCount;

getZeros(ZeroCount, RegisterCount, Name) -> TempValue = getRegisterValue(RegisterCount, Name),
									  case TempValue of 
										  0 -> getZeros(ZeroCount + 1, RegisterCount - 1, Name);
										  _ -> getZeros(ZeroCount, RegisterCount -1, Name)
									  end.
									  
getRegisterValue(RegisterCount, Name) ->
	list_to_atom(string:concat(Name ,integer_to_list(RegisterCount))) ! {self(), getValue},
	%%list_to_atom(integer_to_list(RegisterCount)) ! {self(), getValue},
	receive 
		Result -> Result 
	end.

pushElementToRegister(Rank, Rest, Name) ->
	list_to_atom(string:concat(Name ,integer_to_list(Rank))) ! {self(), setValue, Rest},
	%%list_to_atom(integer_to_list(Rank)) ! {self(), setValue, Rest},
	receive
		Result -> Result
	end.	

