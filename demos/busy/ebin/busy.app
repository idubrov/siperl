{application,busy,
             [{description,"SIP UAS that always responds with 486 Busy Here"},
              {vsn,"0.1.0"},
              {applications,[kernel,stdlib,gproc,sip]},
              {mod,{busy_app,[]}},
              {start_phases,[]},
              {env,[]},
              {modules,[busy_app,busy_sup,busy_uas]}]}.
