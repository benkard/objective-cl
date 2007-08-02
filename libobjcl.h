/* -*- mode: objc; coding: utf-8 -*- */

void
objcl_initialise_runtime ();

void
objcl_shutdown_runtime ();

void *
objcl_invoke_instance_method (void *receiver,
                              char *const method_name,
                              int argc,
                              ...);

void *
objcl_invoke_class_method (void *class,
                           char *const method_name,
                           int argc,
                           ...);

void *
objcl_find_class (char *const class_name);
