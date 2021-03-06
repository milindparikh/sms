-module(bloomfunc).
-export([calc_least_elements/2, calc_hash_indices/3]).
-export ([set_bits/4, get_bits/3, decr_bits/3, decr_p_counters/4]).

-import(math, [log/1, pow/2]).
-import(erlang, [phash2/2]).


calc_least_elements (N, E) -> calc_least_elements(N, E, 1, 0, 0).
calc_least_elements (N, E, NumberOfHashFunctions, MinSizeOfMap, BestNumberOfHashFunctions) -> 
    SizeOfMap = -1 * NumberOfHashFunctions * N /  log(1 - pow(E, 1/NumberOfHashFunctions)),
     {CurSizeOfMap, CurNumberOfHashFunctions} = 
     if SizeOfMap < MinSizeOfMap -> {SizeOfMap, NumberOfHashFunctions}; true -> {MinSizeOfMap, BestNumberOfHashFunctions} end,
     case NumberOfHashFunctions of 
        1 -> calc_least_elements(N, E, NumberOfHashFunctions+1, SizeOfMap, NumberOfHashFunctions);
       100 -> {trunc(CurSizeOfMap) + 1, CurNumberOfHashFunctions};
       _ -> calc_least_elements(N, E, NumberOfHashFunctions+1, CurSizeOfMap, CurNumberOfHashFunctions)
    end.


calc_hash_indices (Key, SizeOfMap, NumberOfHashFunctions ) -> 
   X = phash2 (Key, SizeOfMap),
   Y = phash2 ({"salt", Key}, SizeOfMap),
   calc_hash_indices (SizeOfMap, NumberOfHashFunctions - 1, X, Y, [X]).

calc_hash_indices (_, 0, _, _, AccumulatedIndices) -> AccumulatedIndices;
calc_hash_indices (SizeOfMap, NumberOfHashFunctions, X, Y, AccumulatedIndices) -> 
    Xi = (X+Y) rem SizeOfMap,
    Yi = (Y+NumberOfHashFunctions) rem SizeOfMap,
    calc_hash_indices (SizeOfMap, NumberOfHashFunctions - 1, Xi, Yi, [Xi | AccumulatedIndices]).


set_bits(Bin, _, [], _) -> Bin;
set_bits(Bin, Size, [Index | T], Value) -> 
	 PreBits = Size * (Index - 1),
         <<Pre:PreBits/bits, _:Size, Post/bits>> = Bin,
	 set_bits(<<Pre:PreBits/bits, Value:Size, Post/bits>>, Size, T, Value).	

get_bits(Bin, Size, Index) -> 
	 PreBits = Size * (Index - 1),
	 <<_:PreBits/bits, RealBits:Size, _/bits>> = Bin,	
	 RealBits.

decr_bits(Bin, Size, Index) ->
	 PreBits = Size * (Index - 1),
	 <<Pre:PreBits/bits, RealBits:Size, Post/bits>> = Bin,	
	 case RealBits == 0 of 
            true -> Bin;
            false -> DecrBits = (RealBits - 1), 
                     <<Pre:PreBits/bits, DecrBits:Size, Post/bits>>
            end.
         

decr_p_counters(Bin, _, _, 0) -> Bin;
decr_p_counters(Bin, StartPosition, MaxSizeOfInt, DecrCount) ->
        Bin0 = decr_bits(Bin, MaxSizeOfInt, (StartPosition - DecrCount)),
	decr_p_counters(Bin0, StartPosition, MaxSizeOfInt, DecrCount -1 ).

