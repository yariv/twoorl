{application, twoorl, [
  {description, "Twoorl is an open-source Twitter clone."},
  {vsn, "0.3"},
  {modules, [
    twoorl,
    twoorl_server,
    twoorl_sup
  ]},
  {registered, [twoorl]},
  {env, []},
  {applications, [kernel, stdlib, sasl, crypto, inets, mnesia]},
  {mod, {twoorl, []}},
  {start_phases, []}
]}.
