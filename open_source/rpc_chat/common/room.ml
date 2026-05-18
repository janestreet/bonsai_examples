open! Core

include
  (val String_id.make
         ~caller_identity:String_id.legacy_identity
         ~module_name:"Bonsai_chat_common.Room"
         ~include_default_validation:true
         ())
