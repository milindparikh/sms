%% @doc Implementation of the Hyper Log Log Datastructure
%% @reference [http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf]

-module(hyperloglog).
-export([new/1,  add_element/2, count_elements/1]).
-export([hll_compute_alphamm/2,hll_compute_rank/3, hll_compute_rank/1]).

-import(math, [log/1, pow/2]).
-import(erlang, [phash2/2]).
-import(bloomfunc, [ set_bits/4, get_bits/3]).

			

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
	 Log2M = trunc(log((1.106 / Rsd)*(1.106/ Rsd))/ log(2)), 
         M = trunc(pow(2, Log2M)),
	 AlphaMM = hll_compute_alphamm(Log2M, M),
	 SizeOfInt = trunc(log(32 - Log2M)/ log(2)),	
         SizeOfBitMap = M*SizeOfInt,
	 #hyperloglog{log2m = Log2M, m = M, alphamm = AlphaMM, sizeofint=SizeOfInt, bitmap = <<0:SizeOfBitMap>>}.


count_elements(#hyperloglog{alphamm = AlphaMM} = HLL) -> 
      EstimateFromBitMap = hll_compute_sum_in_bit_map(HLL),
      RevisedEstimate = (AlphaMM * (1 / EstimateFromBitMap)),
      ReRevisedEstimate = hll_apply_range_estimators (RevisedEstimate, HLL),
      ReRevisedEstimate.



add_element(Key, #hyperloglog{log2m = Log2M, bitmap = BitMap, sizeofint = SizeOfInt} = HLL) ->
        BinHash = erlang:phash2(Key, 4294967296),		 
	<<Idx:Log2M, Rest/bits>> = <<BinHash:32>>,
        Rank = hll_compute_rank(Rest),
	CurRankInIdx = get_bits(BitMap, SizeOfInt, Idx),
        case (CurRankInIdx < Rank) of 
                  true -> 
                      io:fwrite("Something is going on"),
                      BitMap0 = set_bits(BitMap, SizeOfInt, [Idx], Rank),
          	      HLL#hyperloglog{bitmap=BitMap0};
                   false -> 
                      HLL
	end.
            
               
hll_compute_sum_in_bit_map(#hyperloglog{m=M, bitmap=BitMap, sizeofint=SizeOfInt}) -> 
               hll_compute_sum_in_bit_map(BitMap, SizeOfInt, M, 0).

hll_compute_sum_in_bit_map(_, _, 0, Estimate) -> Estimate;
hll_compute_sum_in_bit_map(Bin, SizeOfInt, Cnt, Estimate) -> 
      Value = get_bits(Bin, SizeOfInt, Cnt),
      hll_compute_sum_in_bit_map(Bin, SizeOfInt, Cnt - 1, Estimate+pow(2, (-1 * Value))).


hll_compute_rank(Bin) -> 
     	   <<F1:1, R1/bits>> = Bin,
	   hll_compute_rank(F1, R1, 1).

hll_compute_rank(F, <<>>, K) -> 
      case F of 
         1 -> K;
         0 -> K+1
      end;

hll_compute_rank(F, <<R/bits>>, K) -> 
      case F of 
         1 -> K;
         0 -> 
	   <<F1:1, R1/bits>> = R, 
           hll_compute_rank (F1, R1, (K+1))
      end.
     
        

hll_compute_alphamm(Log2M, M) ->
     case Log2M of 
       4 -> (0.673 * M * M);
       5 -> (0.697 * M * M);
       6 -> (0.709 * M * M);
       _ -> (0.7213 / (1 + 1.079 / M)) * M * M
      end.

%% Short Range Estimator
hll_apply_range_estimators (Estimate, #hyperloglog{m = M, bitmap=BitMap, sizeofint = SizeOfInt}) when (Estimate =< (5.0/2.0) * M) -> 
      NumberOfZerosInBitMap = hll_compute_zeros_in_bitmap(BitMap, SizeOfInt, M),
      round(M * log(M/NumberOfZerosInBitMap));

hll_apply_range_estimators (Estimate,_) -> round(Estimate).



hll_compute_zeros_in_bitmap(BitMap, SizeOfInt, Count) ->
     hll_compute_zeros_in_bitmap(BitMap, SizeOfInt, Count, 0).
     hll_compute_zeros_in_bitmap(_, _, 0, Counter) -> Counter;
     hll_compute_zeros_in_bitmap(BitMap, SizeOfInt, Count, Counter) ->
     IncrCount = get_bits(BitMap, SizeOfInt, Count),
     hll_compute_zeros_in_bitmap(BitMap, SizeOfInt, Count-1, Counter +
     case IncrCount == 0 of true -> 1; false -> 0 end).




