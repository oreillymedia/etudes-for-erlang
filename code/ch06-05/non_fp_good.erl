%% @author J D Eisenberg <jdavid.eisenberg@gmail.com>
%% @doc Generate a random set of teeth, with a certain
%% percentage expected to be bad.
%% @copyright 2013 J D Eisenberg
%% @version 0.1

-module(non_fp_teeth).
-export([generate_teeth/2]).

%% @doc Generate a list of lists, six numbers per tooth, giving random
%% pocket depths. Takes a list of booleans (true="there's a tooth there")
%% and a float giving probability that a tooth is good.

-spec(generate_teeth(list(), float()) -> list()).

generate_teeth(TeethPresent, ProbGood) ->
 generate_teeth(TeethPresent, ProbGood, []).

generate_teeth([], _Prob, Result) -> lists:reverse(Result);

generate_teeth([false|Tail], ProbGood, Result) ->
  generate_teeth(Tail, ProbGood, [[] | Result]);
  
generate_teeth([Head|Tail], ProbGood, Result) ->
    generate_teeth(Tail, ProbGood,
    [generate_tooth(Head, ProbGood) | Result]).

generate_tooth(Present, ProbGood) ->
  Good = random:uniform() < ProbGood,
  case Present of
    true -> 
      case Good of
        true -> BaseDepth = 10 + random:uniform(20);
        false -> BaseDepth = 25 + random:uniform(12)
      end,
      generate_tooth(BaseDepth, 6, []);
    false -> []
  end.

generate_tooth(Base, 0, Result) -> Result;

generate_tooth(Base, N, Result) ->
  [(Base + random:uniform(5)) / 10.0 | generate_tooth(Base, N - 1, Result)].  
