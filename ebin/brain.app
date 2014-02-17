{application,brain,
             [{description,"The brain is a distributed Tuplespace"},
              {vsn,"0.0.2"},
              {registered,[brain]},
              {applications,[kernel,stdlib]},
              {mod,{brain_app,[]}},
              {env,[]},
              {modules,[brain_app,brain_sup,reactor,tuple_space,
                        tuple_space_server,utilities]}]}.
