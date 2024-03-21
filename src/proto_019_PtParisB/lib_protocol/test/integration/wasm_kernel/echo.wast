(module

 (type $read_t (func (param i32 i32 i32) (result i32)))
 (type $write_t (func (param i32 i32) (result i32)))
 (type $store_w_t (func (param i32 i32 i32 i32 i32) (result i32)))

 (import "smart_rollup_core" "read_input" (func $read_input (type $read_t)))
 (import "smart_rollup_core" "write_output" (func $write_output (type $write_t)))
 (import "smart_rollup_core" "store_write"
         (func $store_write (type $store_w_t)))

 (data (i32.const 100) "/kernel/env/reboot")
 (data (i32.const 120) "\00\01") ;;Start_of_level
 (data (i32.const 122) "\00\02") ;;End_of_level
 (data (i32.const 124) "\00\00") ;;Internal Transfer
 (data (i32.const 126) "\01") ;;External

 (memory 1)
 (export "mem" (memory 0))

 (func $set_reboot_flag (param $input_offset i32) ;;location of input
       (local $eol i32)
       (local $input_header i32)

       (local.set $eol (i32.load16_u (i32.const 122)))
       (local.set $input_header
                  (i32.load16_u (local.get $input_offset)))
       (i32.ne (local.get $eol) (local.get $input_header))
       (if (then
            (call $store_write
                  (i32.const 100) ;; offset
                  (i32.const 18) ;; key size
                  (i32.const 0) ;; offset in the durable storage page
                  (i32.const 100) ;; offset in memory for the value (placeholder here)
                  (i32.const 0)) ;; size of the value in memory (placeholder here)
            (drop)))
       )

 ;; Internal message representation
 ;; (see Data_encoding.Binary.describe Sc_rollup_inbox_message_repr.encoding):
 ;; - Tag (1B) `t1`
 ;; - Tag (1B) `t2`
 ;; - Payload (variable) `payload`, expected as a Byte
 ;;   + Tag (1B) `tb`
 ;;   + Size (4B) `size_b`
 ;;   + bytes (variable)
 ;; - Sender (20B) `sender`
 ;; - Source (21B) `source`
 ;; - Destination (20B) `destination`
 ;;
 ;; payload = len - (t1 + t2 + tb + size_b + sender + source + destination)
 ;; ==> payload = len - (1 + 1 + 1 + 4 + 20 + 21 + 20)
 ;; ==> payload = len - 68
 ;; and starts at offset 7 from the input

 (func $internal_payload_size (param $input_size i32) (result i32)
       (i32.sub (local.get $input_size) (i32.const 68))) ;; tag

 (func $write_message (param $input_offset i32) (param $size i32)
       (local $internal i32)
       (local $external i32)
       (local $message_tag i32)
       (local $internal_transfer_tag i32)
       (local $payload_size i32)

       (local.set $external (i32.load8_u (i32.const 126)))
       (local.set $internal (i32.load16_u (i32.const 124)))
       (local.set $message_tag
                  (i32.load8_u (local.get $input_offset)))
       (local.set $internal_transfer_tag
                  (i32.load16_u (local.get $input_offset)))
       (local.set $payload_size
                  (call $internal_payload_size (local.get $size)))

       (if
        (i32.eq (local.get $message_tag) (local.get $external))
        (then
         (call $write_output
               (i32.add (local.get $input_offset) (i32.const 1)) ;;Remove the header
               (i32.sub (local.get $size) (i32.const 1))) ;;Size without the header
         (drop))
        (else
         (if
          (i32.eq (local.get $internal_transfer_tag) (local.get $internal))
          (then
           (call $write_output ;;See comment for the internal message representation
                 (i32.add (local.get $input_offset) (i32.const 7))
                 (local.get $payload_size))
           (drop))
          )
         )
        )
       )

 (func (export "kernel_run")
       (local $size i32)
       (local.set $size (call $read_input
                              (i32.const 220) ;; info_addr
                              (i32.const 260) ;; dst
                              (i32.const 3600))) ;; max_bytes

       (call $write_message (i32.const 260)
             (local.get $size))
       (call $set_reboot_flag (i32.const 260))
       )
)
