open! Core

include
  (val String_id.make
         ~module_name:"Bonsai_chat_common.Room"
         ~include_default_validation:true
         ())
