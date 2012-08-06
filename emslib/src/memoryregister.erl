
%% Created: Jul 2, 2012
%% Description: TODO: Add description to register
-module(memoryregister).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([calculation/1]).

%%
%% API Functions
%%

calculation(Value)->
	receive
		{Parent, getValue} -> 
			Parent ! Value,
			calculation(Value);
		{Parent, initialize} ->
			calculation(0),
			Parent ! {initialized};
		{Parent, setValue, NewValue} ->
 			Sigma = calculateSigma(NewValue),
 			case Sigma > Value of 
                   true ->   
					 Parent ! {Sigma, elementAdded},
                     calculation(Sigma);
                   _ -> Parent ! {Sigma, noNeedToAddElement},
						calculation(Value)
 			end
	end.

%%
%% Local Functions
%%

%%  Get first bit position whose value is 1. calculateSigma(0001) = 4
calculateSigma(Bin) -> 
     	   <<F1:1, R1/bits>> = Bin,
	   calculateSigma(F1, R1, 1).
calculateSigma(F, <<>>, K) -> 
      case F of 
         1 -> K;
         0 -> K+1
      end;
calculateSigma(F, <<R/bits>>, K) -> 
      case F of 
         1 -> K;
         0 -> 
	   <<F1:1, R1/bits>> = R, 
           calculateSigma (F1, R1, (K+1))
      end.



