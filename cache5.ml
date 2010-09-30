(*
 *
 * Copyright (c) 2009-, 
 *  Shuchang Zhou    <zhoushuchang@ict.ac.cn>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open List
open Printf

type address = int

module BoundMap = struct
  type t = {
    mutable h' : (address, float) Hashtbl.t;
    mutable h'' : (address, float) Hashtbl.t;
    mutable offset : float;
    threshold : float;
    mutable c : float;
  }
  let create n epsilon = {
    h' = Hashtbl.create n;
    h'' = Hashtbl.create n;
    offset = 0.;
    threshold = log epsilon /. (log ( 1. -. 1. /. float n));
    c = 0.;
  }
  let get t addr =
    try Some (Hashtbl.find t.h' addr +. t.offset)
    with Not_found ->
        try Some (Hashtbl.find t.h'' addr +. t.offset)
        with Not_found -> None
  
  let set t addr value =
    Hashtbl.replace t.h' addr (value -. t.offset)
  
  let swap t =
    Hashtbl.clear t.h'';
    let h'' = t.h'' in
    t.h'' <- t.h';
    t.h' <- h''
  
  let addOffset t off =
    t.offset <- t.offset +. off;
    t.c <- t.c +. off;
    if t.c > t.threshold then begin
      swap t;
      t.c <- t.c -. t.threshold
    end

end
module M = BoundMap
module ProbCache = struct
  type t = {
    map : M.t;
    accept : address -> float;
  }
  let create epsilon size =
    let ratio = 1. -. 1. /. float size in
    let m = M.create size epsilon in
    { map = m;
      accept = fun addr ->
        let exi = match M.get m addr with
          | Some esum -> 1. -. ratio ** esum
          | None -> 1. in
        M.addOffset m exi;
        M.set m addr 0.;
        exi
      }
  let accept t address = t.accept address
end

let () =
  let cache = ProbCache.create 0.1 4 in
  let trace = map int_of_char ['a';'b';'a';'c';'d';'b'] in
  let miss_probs = map (ProbCache.accept cache) trace in
  iter (printf "%f,") miss_probs;print_newline ()