% Copyright 2013 and onwards Roman Gafiyatullin
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% 
% See the NOTICE file distributed with this work for additional information regarding copyright ownership.
% 

-module(jd_token_bucket).
-export([
		new/3,
		consume/2
	]).

-record( tb, {
		bucket_size :: pos_integer(),
		cooldown :: pos_integer(),
		refill_rate :: pos_integer(),

		bucket_level :: integer(),
		last_refill_time :: pos_integer()
	} ).
-opaque tb() :: #tb{}.

-spec new(
		BucketSize :: pos_integer(),
		Cooldown :: pos_integer(),
		RefillRate :: pos_integer()
	) -> tb().
-spec consume(
		TokensCount :: non_neg_integer(),
		ContextIn :: tb()
	) -> ContextOut :: tb().

new( BucketSize, Cooldown, RefillRate ) ->
	#tb{
		bucket_size = BucketSize, 
		cooldown = Cooldown,
		refill_rate = RefillRate,

		bucket_level = BucketSize,
		last_refill_time = now_s()
	}.

consume( TokensCount, TBIn = #tb{} ) ->
	TBRefilled = refill_bucket( TBIn ),
	BucketLevel = (TBRefilled #tb.bucket_level) - TokensCount,
	TBConsumed = TBRefilled #tb{ bucket_level = BucketLevel },
	cooldown( TBConsumed ).

%%% %%%%%%%% %%%
%%% Internal %%%
%%% %%%%%%%% %%%

cooldown( TB = #tb{ bucket_level = BL } ) when BL >= 0 -> TB;
cooldown( TB = #tb{ cooldown = CD, bucket_level = BL } ) when BL < 0 ->
	timer:sleep( CD * 1000 ),
	TBRefilled = refill_bucket( TB ),
	cooldown( TBRefilled ).

refill_bucket(
	TB = #tb{
		bucket_size = BS,
		bucket_level = BL,
		last_refill_time = LastRefillTime,
		refill_rate = RefillRate
	}
) ->
	Now = now_s(),
	Delta = Now - LastRefillTime,
	RefillBy = Delta * RefillRate,
	TB #tb{
		last_refill_time = Now,
		bucket_level = min(BS, BL + RefillBy)
	}.

now_s() -> jd_time:to_s(jd_time:now()).
