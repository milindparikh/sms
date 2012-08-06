%% 
%% Created: Jul 2, 2012
%% Description: TODO: Add description to HyperLogLog
%% TODO: 
%%  -> Named HyperLogLog
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

initialize(RSD) -> initialize(RSD, createRegisters).

initialize(RSD, Type) -> Log2M = log2M(RSD),
				   RegisterCount = trunc(math:pow(2, Log2M)),
				   case Type of 
			  		  createRegisters -> 
				  		destroyExistingRegisters(RegisterCount);
					  _ -> ok
		  		   end,
				   AlphaMM = getAlphaMM(Log2M, RegisterCount),
				   initialize_registers(RegisterCount, Type),
				   spawn(eHyperLogLog, initialize, [Log2M, RegisterCount, AlphaMM]).			   

initialize(Log2M, RegisterCount, AlphaMM) ->
	              receive
					    {Parent, addElement, Element} -> Parent ! add_element(Element, Log2M),
									initialize(Log2M, RegisterCount, AlphaMM);
					    {Parent, getCardinality} -> Parent ! cardinality(RegisterCount, AlphaMM),
				  					initialize(Log2M, RegisterCount, AlphaMM)
                   end.


%%
%% Local Functions
%%

initialize_registers(0, Type) -> io:format("~n Registers Initialized via ~p", [Type]);

initialize_registers(RegisterCount, Type)
     when RegisterCount > 0 ->
	      RegisterName = list_to_atom(integer_to_list(RegisterCount)),
		  setRegister( RegisterName, Type),
		  initialize_registers(RegisterCount - 1, Type).
    

add_element(Element, Log2M) ->
	BinHash = erlang:phash2(Element, 4294967296),
	<<Idx:Log2M, Rest/bits>> = <<BinHash:32>>,
	Rank = computeRank(Idx),
	Result = pushElementToRegister(Rank, Rest),
	list_to_atom(integer_to_list(Rank)) ! {setValue, Rest},
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
	
cardinality(RegisterCount, AlphaMM) -> 
							  RegisterSum = getRegisterSum(0 , RegisterCount),
 							  Estimate = getEstimate(RegisterSum, AlphaMM),
 							  RevisedEstimate = getRevisedSmallRangeEstimate(RegisterCount),
 							  RevisedEstimate.

getRegisterSum(RegisterSum, 0) -> RegisterSum;

getRegisterSum(RegisterSum, RegisterCount) -> 
											  TempRegisterSum = RegisterSum + math:pow(2, getRegisterValue(RegisterCount)),
											  getRegisterSum(TempRegisterSum, (RegisterCount-1)).

getEstimate(RegisterSum, AlphaMM) -> AlphaMM * (1/ RegisterSum).

getRevisedSmallRangeEstimate(RegisterCount) -> Zeros = getZeros(0, RegisterCount),
											   trunc(RegisterCount * math:log( RegisterCount/ Zeros)).

getZeros(ZeroCount, 0) -> ZeroCount;

getZeros(ZeroCount, RegisterCount) -> TempValue = getRegisterValue(RegisterCount),
									  case TempValue of 
										  0 -> getZeros(ZeroCount + 1, RegisterCount - 1);
										  _ -> getZeros(ZeroCount, RegisterCount -1)
									  end.
									  
getRegisterValue(RegisterCount) ->
	list_to_atom(integer_to_list(RegisterCount)) ! {self(), getValue},
	receive 
		Result -> Result 
	end.

pushElementToRegister(Rank, Rest) ->
	list_to_atom(integer_to_list(Rank)) ! {self(), setValue, Rest},
	receive
		Result -> Result
	end.	

