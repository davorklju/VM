.text
    :main   move.8 r0 data
            move.8 r1 len
            load.8 r1 r1 0
            call sort
            halt

////////////////////////////////////////////////////////////////////////////////////////
    :min_index                //r0 = start index
            move.8 rv r0      //r1 = end index
    :min_index_loop           //rv = best index
            cmp.8 r0 r1
            jge.8 min_index_loop_end

            load.8 r2 rv 0    //r2 = best value
            load.8 r3 r0 0    //r3 = current value
            cmp.8 r2 r3
            jle.8 keep        //if best val <= current val then keep
            move.8 rv r0      // else best idx = current idx
    :keep   add.8 r0 1
            jmp.8 min_index_loop
    :min_index_loop_end
            ret
////////////////////////////////////////////////////////////////////////////////////////
    :swap
            load.8  r2 r0 0
            load.8  r3 r1 0
            store.8 r0 r3 0
            store.8 r1 r2 0
            ret
////////////////////////////////////////////////////////////////////////////////////////
    :sort   push fp
            move fp sp              //r0 = data start
                                                //r1 = len
            move.8 r2 0             //r2 = loop index
    :sort_loop
            cmp.8 r2 r1
            jge.8 sort_loop_end     //ret when loop index >= len

            push.8 r0               //save r0,r1,r2 for call
            push.8 r1
            push.8 r2

            add.8 r1 r0             //r1 = end index
            add.8 r0 r2             //r0 = current index
            call min_index          //rv = min index

            load.8 r0 fp 0          //restore r0,r1,r2 from stack
            load.8 r1 fp 1          //while preserving for call
            load.8 r2 fp 2

            add.8 r0 r2             //r0 = current index
            move.8 r1 rv            //r1 = min index
            call swap

            pop.8 r2
            pop.8 r1
            pop.8 r0

            add.8 r2 1             // inc loop index
            jmp.8 sort_loop
    :sort_loop_end
            move sp fp
            pop fp
            ret
////////////////////////////////////////////////////////////////////////////////////////

.data
    :data   i8      9 5 3 1 10 0 6 2 7 4 8
    :len    i8     11
