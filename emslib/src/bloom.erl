%% @doc Implementation of the Bloom filter data structure.
%% @reference [http://en.wikipedia.org/wiki/Bloom_filter]

-module(bloom).
-export([new/1, new/2, is_bloom/1, is_element/2, add_element/2]).

-import(math, [log/1, pow/2]).
-import(erlang, [phash2/2]).
-import(bloomfunc, [calc_least_elements/2, calc_hash_indices/3, set_bits/4, get_bits/3]).
-import(bloomfunc, [decr_p_counters/4]).
			

-record(bloom, {
    m      = 0,       % The size of the bitmap in bits.
    bitmap = <<>>,    % The bitmap.
    k      = 0,       % The number of hashes.
    n      = 0,       % The maximum number of keys.
    keys   = 0        % The current number of keys.
}).

%% @spec new(capacity) -> bloom()
%% @equiv new(capacity, 0.001)
new(N) -> new(N, 0.001).

%% @spec new(integer(), float()) -> bloom()
%% @doc Creates a new Bloom filter, given a maximum number of keys and a
%%     false-positive error rate.
%%     M is the number of bits required to represent the bloom filter
%%     K is the number of hashfunctions required to set bits to one
%%     bitmap is a multiple of eight instead of exact
 
new(N, E) when N > 0, is_float(E), E > 0, E =< 1 ->
    {M, K} = calc_least_elements(N, E),
    #bloom{m=M, bitmap = <<0:M>>, k=K, n=N}.

%% @spec is_bloom(bloom()) -> bool()
%% @doc Determines if the given argument is a bloom record.
is_bloom(#bloom{}) -> true;
is_bloom(_) -> false.

%% @spec is_element(string(), bloom()) -> bool()
%% @doc Determines if the key is (probably) an element of the filter.
is_element(Key, B) -> is_element(Key, B, calc_hash_indices(Key, B#bloom.m, B#bloom.k)).

is_element(_, _, []) -> true;
is_element(Key, B, [Idx | T]) ->
    RealBits = get_bits(B#bloom.bitmap, 1, Idx),
    case 1 == RealBits of
         true -> is_element(Key, B, T);
        false -> false
    end.

%% @spec add_element(string(), bloom()) -> bloom()
%% @doc Adds the key to the filter.
add_element(Key, #bloom{k = K, m = M, keys=Keys, n=N, bitmap=Bitmap} = B) when Keys < N ->
    Idxs = calc_hash_indices(Key, M, K),
    Bitmap0 = set_bits(Bitmap,1, Idxs,1),
    case Bitmap0 == Bitmap of
         true -> B;    % Don't increment key count for duplicates.
        false -> B#bloom{bitmap=Bitmap0, keys=Keys+1}
    end.

