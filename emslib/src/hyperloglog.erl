%% @doc Implementation of the Hyper Log Log Datastructure
%% @reference [http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf]

-module(hyperloglog).
-export([new/1,  add_element/2, count_elements/1, calculateLog2M/1]).
-export([computeAlphaMM/2,computeRank/3, computeRank/1, check/0]).

-import(math, [log/1, pow/2]).
-import(erlang, [phash2/2]).
-import(bloomfunc, [ set_bits/4, get_bits/3]).
-include_lib("eunit/include/eunit.hrl").	

-record(hyperloglog, {
    log2m  = 0,       % The number of bits in HLL.. m = pow(2, log2m)
    m      = 0,       % The size of the bitmap in bits.
    sizeofint = 0,    % size of int in bitmap
    alphamm = 0,
    bitmap = <<>>    % The bitmap.


}).

%% @spec new(capacity) -> bloom()
%% @equiv new(capacity, 0.001)
new(Rsd) -> 
	 Log2M = calculateLog2M(Rsd), 
     M = trunc(pow(2, Log2M)),
	 AlphaMM = computeAlphaMM(Log2M, M),
	 SizeOfInt = trunc(log(32 - Log2M)/ log(2)),	
     SizeOfBitMap = M*SizeOfInt,
	 #hyperloglog{log2m = Log2M, m = M, alphamm = AlphaMM, sizeofint=SizeOfInt, bitmap = <<0:SizeOfBitMap>>}.



count_elements(#hyperloglog{log2m = Log2M, alphamm = AlphaMM} = HLL) -> 
      EstimateFromBitMap = hll_compute_sum_in_bit_map(HLL),
      RevisedEstimate = (AlphaMM * (1 / EstimateFromBitMap)),
      Log2M.


add_element(Key, #hyperloglog{log2m = Log2M, bitmap = BitMap, sizeofint = SizeOfInt} = HLL) ->
    BinHash = erlang:phash2(Key, 4294967296),		 
	<<Idx:Log2M, Rest/bits>> = <<BinHash:32>>,
    Rank = computeRank(Rest) + 1,
	CurRankInIdx = get_bits(BitMap, SizeOfInt, Idx),
        case (CurRankInIdx < Rank) of 
                  true -> 
                      io:fwrite("Something is going on"),
                      BitMap0 = set_bits(BitMap, SizeOfInt, [Idx], Rank),
          	      HLL#hyperloglog{bitmap=BitMap0};
                   false -> 
                      HLL
	end.
        
check() -> <<Idx:26, Rest/bits>> = <<1278007117:32>>, {Idx, Rest}.  
               
hll_compute_sum_in_bit_map(#hyperloglog{m=M, bitmap=BitMap, sizeofint=SizeOfInt}) -> 
               hll_compute_sum_in_bit_map(BitMap, SizeOfInt, M, 0).

hll_compute_sum_in_bit_map(_, _, 0, Estimate) -> Estimate;
hll_compute_sum_in_bit_map(Bin, SizeOfInt, Cnt, Estimate) -> 
      Value = get_bits(Bin, SizeOfInt, Cnt),
      hll_compute_sum_in_bit_map(Bin, SizeOfInt, Cnt - 1, Estimate+pow(2, (-1 * Value))).


computeRank(Bin) -> 
     	   <<F1:1, R1/bits>> = Bin,
	   computeRank(F1, R1, 1).

computeRank(F, <<>>, K) -> 
      case F of 
         1 -> K;
         0 -> K+1
      end;

computeRank(F, <<R/bits>>, K) -> 
      case F of 
         1 -> K;
         0 -> 
	   <<F1:1, R1/bits>> = R, 
           computeRank (F1, R1, (K+1))
      end.
     
        
%% Compute Alpha and product of M * M 
computeAlphaMM(Log2M, M) ->
     case Log2M of 
       4 -> (0.673 * M * M);
       5 -> (0.697 * M * M);
       6 -> (0.709 * M * M);
       _ -> (0.7213 / (1 + 1.079 / M)) * M * M
      end.


%% Calculate Log2M
calculateLog2M(RSD) -> trunc(log((1.106 / RSD)*(1.106/ RSD))/ log(2)).


%% Eunit tests %% 
%% Eunit test for new hyperlog
new_test() -> 
	?assertEqual(#hyperloglog{log2m = 26, m = 67108864, alphamm = 3248446358992672.0, sizeofint=2, bitmap = <<0:134217728>>}, new(0.0001)).

%% Eunit test for computerAlphaMM
computerAlphaMM_upto4decimal_test() ->
	?assertEqual(3248446358992672.0, computeAlphaMM(26, 67108864)).

%% EUnit test cases for calculateLog2M
%% Test Case with RSD 0001
calculateLog2M_upto4decimal_test() ->
	?assertEqual(26, calculateLog2M(0.0001)).

calculateLog2M_upto3decimal_test() -> 
	?assertEqual(20, calculateLog2M(0.001)).


%% Eunit test for computerRank
computeRank_test() -> 
	?assertEqual(3, computeRank(<<13:6>>)).
