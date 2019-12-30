source /usr/share/gdb/auto-load/usr/lib64/libpython3.6m.so.1.0-gdb.py

add-auto-load-safe-path /usr/lib64/go/src/runtime/runtime-gdb.py


define pyrun
  set $tstate = PyGILState_Ensure()
  call PyRun_SimpleString($arg0)
  call PyGILState_Release($tstate)
end


define pystack
  pyrun "import traceback;print('='*80);traceback.print_stack();del traceback;"
end

define pythread
  pyrun "import threading;print(threading.current_thread());del threading;"
end
