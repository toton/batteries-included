(*
 * Uref -- unifiable references
 * Copyright (C) 2011  Batteries Included Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Implements union-find with ranks and path-compression  *)

type 'a uref_contents =
  | Ranked of 'a * int
  | Ptr of 'a uref
and 'a uref = 'a uref_contents ref

type 'a t = 'a uref

let rec find ur = match !ur with
  | Ptr p ->
      let (vr, _, _) as found = find p in
      ur := Ptr vr ;
      found
  | Ranked (x, rank) -> (ur, x, rank)

let uref x = ref (Ranked (x, 0))

let uget ur =
  let _, x, _ = find ur in
  x

let uset ur x =
  let ur, _, rank = find ur in
  ur := Ranked (x, rank)

let equal ur vr =
  find ur == find vr

let unite ?(sel=(fun x y -> x)) ur vr =
  let ur, x, xr = find ur in
  let vr, y, yr = find vr in
  if ur == vr then () else
    if xr = yr then begin
      ur := Ranked (sel x y, xr + 1) ;
      vr := Ptr ur
    end else if xr < yr then begin
      ur := Ranked (sel x y, xr) ;
      vr := Ptr ur
    end else begin
      vr := Ranked (sel x y, yr) ;
      ur := Ptr vr
    end

let print elepr out ur =
  BatInnerIO.nwrite out "uref " ;
  elepr out (uget ur)

let t_printer elepr paren out ur =
  if paren then BatInnerIO.nwrite out "(" ;
  print (elepr false) out ur ;
  if paren then BatInnerIO.nwrite out ")"

let uref_printer = t_printer
