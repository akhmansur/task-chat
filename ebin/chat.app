{application,chat,
             [{description,"Simple chat prg using erlbus"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{chat_app,[]}},
              {env,[]},
              {modules,[chat_app,chat_cowboy_ws_handler,chat_ebus_handler,
                        chat_sup]}]}.
