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

-module(jd_hexutil).
%%% Seen at http://stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
-compile([native, {hipe, [o3]}]).

-export([b2h/1, h2b/1]).

-spec b2h( Binary :: binary() ) -> Hex :: binary().
b2h(B) when is_binary(B) ->
  bin_to_hex(B, <<>>).

h2b(H) when is_binary(H) ->
  hex_to_bin( H, <<>> ).

hex_to_bin( <<>>, Acc ) -> Acc;
hex_to_bin( << HungDigit:8 >>, _Acc ) -> error( {badarg, {hung_digit, HungDigit}} );
hex_to_bin( <<A:8, B:8, Rest/binary>>, Acc ) ->
  Char = hex_digit_value( A ) * 16 + hex_digit_value( B ),
  hex_to_bin( Rest, << Acc/binary, Char:8 >> ).

hex_digit_value( $0 ) -> 16#0;
hex_digit_value( $1 ) -> 16#1;
hex_digit_value( $2 ) -> 16#2;
hex_digit_value( $3 ) -> 16#3;
hex_digit_value( $4 ) -> 16#4;
hex_digit_value( $5 ) -> 16#5;
hex_digit_value( $6 ) -> 16#6;
hex_digit_value( $7 ) -> 16#7;
hex_digit_value( $8 ) -> 16#8;
hex_digit_value( $9 ) -> 16#9;
hex_digit_value( $a ) -> 16#a;
hex_digit_value( $b ) -> 16#b;
hex_digit_value( $c ) -> 16#c;
hex_digit_value( $d ) -> 16#d;
hex_digit_value( $e ) -> 16#e;
hex_digit_value( $f ) -> 16#f;
hex_digit_value( $A ) -> 16#a;
hex_digit_value( $B ) -> 16#b;
hex_digit_value( $C ) -> 16#c;
hex_digit_value( $D ) -> 16#d;
hex_digit_value( $E ) -> 16#e;
hex_digit_value( $F ) -> 16#f;
hex_digit_value( Invalid ) -> error( {badarg, {invalid_hex_digit, Invalid}} ).


-define(H(X), (hex(X)):16).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(Bin, Acc) when byte_size(Bin) band 7 =:= 0 ->
  bin_to_hex_(Bin, Acc);
bin_to_hex(<<X:8, Rest/binary>>, Acc) ->
  bin_to_hex(Rest, <<Acc/binary, ?H(X)>>).

bin_to_hex_(<<>>, Acc) -> Acc;
bin_to_hex_(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acc) ->
  bin_to_hex_(
    Rest,
    <<Acc/binary,
      ?H(A), ?H(B), ?H(C), ?H(D), ?H(E), ?H(F), ?H(G), ?H(H)>>).

-compile({inline, [hex/1]}).
hex(X) ->
  element(
    X+1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
          16#3037, 16#3038, 16#3039, 16#3061, 16#3062, 16#3063, 16#3064,
          16#3065, 16#3066, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
          16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3161, 16#3162,
          16#3163, 16#3164, 16#3165, 16#3166, 16#3230, 16#3231, 16#3232,
          16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
          16#3261, 16#3262, 16#3263, 16#3264, 16#3265, 16#3266, 16#3330,
          16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
          16#3338, 16#3339, 16#3361, 16#3362, 16#3363, 16#3364, 16#3365,
          16#3366, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
          16#3436, 16#3437, 16#3438, 16#3439, 16#3461, 16#3462, 16#3463,
          16#3464, 16#3465, 16#3466, 16#3530, 16#3531, 16#3532, 16#3533,
          16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3561,
          16#3562, 16#3563, 16#3564, 16#3565, 16#3566, 16#3630, 16#3631,
          16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
          16#3639, 16#3661, 16#3662, 16#3663, 16#3664, 16#3665, 16#3666,
          16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
          16#3737, 16#3738, 16#3739, 16#3761, 16#3762, 16#3763, 16#3764,
          16#3765, 16#3766, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
          16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3861, 16#3862,
          16#3863, 16#3864, 16#3865, 16#3866, 16#3930, 16#3931, 16#3932,
          16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
          16#3961, 16#3962, 16#3963, 16#3964, 16#3965, 16#3966, 16#6130,
          16#6131, 16#6132, 16#6133, 16#6134, 16#6135, 16#6136, 16#6137,
          16#6138, 16#6139, 16#6161, 16#6162, 16#6163, 16#6164, 16#6165,
          16#6166, 16#6230, 16#6231, 16#6232, 16#6233, 16#6234, 16#6235,
          16#6236, 16#6237, 16#6238, 16#6239, 16#6261, 16#6262, 16#6263,
          16#6264, 16#6265, 16#6266, 16#6330, 16#6331, 16#6332, 16#6333,
          16#6334, 16#6335, 16#6336, 16#6337, 16#6338, 16#6339, 16#6361,
          16#6362, 16#6363, 16#6364, 16#6365, 16#6366, 16#6430, 16#6431,
          16#6432, 16#6433, 16#6434, 16#6435, 16#6436, 16#6437, 16#6438,
          16#6439, 16#6461, 16#6462, 16#6463, 16#6464, 16#6465, 16#6466,
          16#6530, 16#6531, 16#6532, 16#6533, 16#6534, 16#6535, 16#6536,
          16#6537, 16#6538, 16#6539, 16#6561, 16#6562, 16#6563, 16#6564,
          16#6565, 16#6566, 16#6630, 16#6631, 16#6632, 16#6633, 16#6634,
          16#6635, 16#6636, 16#6637, 16#6638, 16#6639, 16#6661, 16#6662,
          16#6663, 16#6664, 16#6665, 16#6666}).