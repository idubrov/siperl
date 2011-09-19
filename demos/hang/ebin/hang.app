{application,hang,
             [{description,"SIP UAC that calls other party, then hangs after 10 seconds of ringing"},
              {vsn,"0.1.0"},
              {applications,[kernel,stdlib,gproc,sip]},
              {mod,{hang_app,[]}},
              {start_phases,[]},
              {env,[]},
              {modules,[hang_app,hang_sup,hang_uac]}]}.
