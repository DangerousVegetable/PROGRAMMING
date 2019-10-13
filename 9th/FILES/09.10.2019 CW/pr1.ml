open String;;
open Unix;;

let copy name_in name_out =
	let in_chan = open_in_bin name_in in();
	
	let in_descr = descr_of_in_channel in_chan in ();
	
	let l = in_channel_length in_chan in ();
	
	let b = Bytes.create l in
	read in_descr b 0 l;

	let out_chan = open_out name_out in();
	let out_descr = descr_of_out_channel out_chan in ();	

	write out_descr b 0 l;
	close_in in_chan;
	close_out out_chan;;

copy (Sys.argv.(1)) (Sys.argv.(2));;