(function($) {
    var WS = null;
    
    var templates = {};

    var graphs = {};
    var charts = {};

    var call = { 
        can: false,
        on: false,
        stream: null,
        peers: {}, 
        PC: window.mozRTCPeerConnection || window.webkitRTCPeerConnection,
        ICE: window.mozRTCIceCandidate || window.RTCIceCandidate,
        SD: window.mozRTCSessionDescription || window.RTCSessionDescription
    };
    navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;

    var notification_state = (Notification && Notification.permission) ? 
        Notification.permission.toLowerCase() : "default";

    var se_current = {id: 0, name: "", params: [], commands: ""};
    var se_max_n = 1;

    var scenario = { id: 0, hosts: [], v: {} };
 
    var RE_IP = new RegExp("^[0-9a-f\.:]*$");
    var RE_ID = new RegExp("^[0-9]*$");

    var MAX_TIMELINE_ELEMENTS = 60;

    function repeat(cb) { setTimeout(cb, 1000); }  

    $.app = {}
    $.app.init = function() {
        ui_init();

        if(!("WebSocket" in window)) ui_status("red", "No WebSocket support");  
        else connect();

        if(navigator.getUserMedia) call.can = true;
    }

    /*
        Transport
    */

    function connect()
    {
        ui_status("yellow", "Connecting");
        if(WS) {
            if(WS.readyState == WS.CONNECTING) return;

            WS.onopen = null;
            WS.onmessage = null;
            WS.onclose = null;
            WS.onerror = null;

            if(WS.readyState == WS.OPEN) WS.close();
        }
        WS = new WebSocket( 
            (window.location.protocol == "https:" ? "wss" : "ws") + 
            "://" + window.location.host + "/ctl");

        WS.onopen = function() { ui_status("green", "Connected"); ui_connect(); };
        WS.onmessage = function(m) { handle_cast(m.data && m.data.length > 1 ? $.parseJSON(m.data) : {req: "none"}); };
        WS.onclose = function() { ui_status("orange", "Disconnected"); repeat(function() { connect(); }); };
        WS.onerror = function() { ui_status("red", "Error"); repeat(function() { connect(); }); };
    }

    function send(m, idem) {
        if(WS && WS.readyState == WS.OPEN) { WS.send(JSON.stringify(m)); }
        else if(idem) repeat(function() { send(m, idem); }); 
    }
    function req(r) { send(r, true); } 
    
    function handle_cast(obj) {
        switch(obj.op) {
            case "info_pnode": ui_pnodes(obj); break;
            case "data_pnode": ui_data("pnode", obj); break;
            case "add_pnode": ui_add_pnode_reply(obj); break; 
            case "drop_pnode": ui_drop_pnode_reply(obj); break; 

            case "scenarios": ui_scenarios(obj); break;
            case "datasource": ui_datasource(obj); break;
            case "exec": ui_exec(obj); break;

            case "peers": ui_peers(obj); break;
            case "call": ui_call(obj); break;

            case "reload": location.reload(); break;

            default: console.log("Got obj.req = " + obj.req + " in income WS message."); break;
        }
    }

    /*
        UI
    */

    function ui_init() { 
        $("._template").map(function(k, v) { templates[($(v).attr("id"))] = $(v).html(); });
        
        $("#_menu a").click(function() { ui_menu(this); });
        $("#_nodes").click();
        $(".ui.dropdown").dropdown();

        $("#peers")
            .click(function() { ui_init_call(null); })
            .popup({ popup: $('#peers_incoming'), on: 'manual' });  
        notification_init();
    };
    function ui_status(t, s) { $("#statusbox").attr("class", "ui " + t + " label").text(s).show(); }

    function ui_menu(self) { 
        $("._tabs").hide(); 
        $("#_menu a").removeClass('active'); 
        $("#" + self.id).addClass('active');
        $("#p" + self.id).show();
    }

    function ui_connect() {
        req({op: "info_pnodes"});
        req({op: "scenarios"});
    }

    function ui_apply_template(t, d) {
        var r = templates[t] ? templates[t] : "";
        $.each(d, function(k, v) {
                var re = new RegExp("@" + k + "@", 'g');
                switch(typeof(v)) {
                    case "number": r = r.replace(re, ("" + v).replace(/(\d{1,3}(?=(\d{3})+(?:\.\d|\b)))/g, "\$1 "));
                    case "string": r = r.replace(re, strip_html(v));
                    case "object": if(v.t) r = r.replace(re, ui_sub(v));
                }            
            });
        return r;
    }
    function ui_sub(obj) {
        switch(obj.ct) {
            case "item": return ui_apply_template(obj.t, obj.item);
            case "list": return obj.item.reduce(function(prev, curr, i, a) {
                    return prev + ui_apply_template(obj.t, curr);
                }, "");
            default: return "";
        }
    }
    function ui_detach(t, d, pf) { $("." + ui_id(t, d) + pf).detach(); };
    function ui_detachi(t, d, pf) { $("#" + ui_id(t, d) + pf).detach(); };
    function ui_id(t, d) { return t + "_" + d.id; }
    function ui_up(p, n) { return n == 0 ? p : ui_up(p.parent(), n - 1); }
    function ui_mark(d, up) { 
        ui_up($(d), up).addClass("error");
        (function(upp){ 
                $(d).focus(function(){ ui_up($(this), upp).removeClass("error"); });
                $(d).click(function(){ ui_up($(this), upp).removeClass("error"); });
            })(up); 
    }


    function ui_data(t, d) {
        var eid = "#" + ui_id(t, d) + (d.ct ? "_" + d.ct : "");
        switch(d.ct) {
            case "dmesg":
                var v = "";
                $(d.v).map(function(_i, i) { v = v + ((i && i.length) ? i + "\n" : ""); });
                $(eid).html(strip_html(v));
                break;
            // case "stat": ui_visual_cpu(t, d); break;
            case "vnodes": 
                if(typeof(d.v) == "object") { ui_vnodes(d); } else {
                    $("#" + ui_id(t, d) + "_vnodes_info").text(strip_html(d.v))
                }; 
                break;
            default: if(d.v) $(eid).html(strip_html(d.v));
        }
    }
   function ui_pnodes(d) {
        if(!d.id) return; 

        ui_detach("pnode", d, ""); 
        ui_detach("pnode", d, "_vnode"); 
 
        d['_id'] = ui_id("pnode", d);
        $("#pnodes").append(ui_apply_template("pnode", d)); 
        $("#nodes_list").append(ui_apply_template("pnode_scenario", d));
    }
    function ui_vnodes(d)
    {
        ui_detachi("pnode", d, "_vnodes tr"); 
        ui_detach("pnode", d, "_vnode");

        $.map(d.v, function(v, k) { 
                v['_id'] = ui_id("pnode", d);
                v['_pid'] = d['id'];
                $("#" + ui_id("pnode", d) + "_vnodes").append(ui_apply_template("vnodes", v));
                if(v.state == "running") $("#nodes_list > ." + ui_id("pnode", d)).after(ui_apply_template("vnode_scenario", v));
            });
    }

    function ui_visual_cpu(t, d) {
        var id = ui_id(t, d);
        if($("#" + id) && d.v) {
            var g = graphs[id] ? graphs[id] : {};
            if(d.v.cpu) {
                var g_cpu = g["cpu"] ? g["cpu"] : [];

                var nd = [
                    ['Time', 'User', 'Nice', 'System', 'IOWait', 'IRQ', 'SoftIRQ']
                ];
                var nda = [0, 1, 2, 4, 5, 6];
                
                var a = [];
                var now = new Date();
                var sum = 0;

                for(i = 0; i < d.v.cpu.length; i++) sum = sum + d.v.cpu[i];
                a.push(now.getMinutes() + ":" + n2d(now.getSeconds()));
                $.map(nda, function(v, _k) { a.push(Math.round(d.v.cpu[v] * 100 / sum)); });

                var am = timeline_add(g_cpu, a); 
                ui_graph_timeline(id + "_cpu", nd.concat(am));

                g["cpu"] = am;
            }
            graphs[id] = g;
        }
    }
    function ui_graph_timeline(t, d) {
        if(!charts[t]) charts[t] = new google.visualization.ColumnChart($("#" + t)[0]);
        charts[t].draw(google.visualization.arrayToDataTable(d), { isStacked: true });
    }

    function timeline_add(a, e) {
        if(a.length >= MAX_TIMELINE_ELEMENTS) a.shift(); 
        a.push(e);   
        return a;
    }
    function num_to_str(d) {
        return d/1000000 > 1 ? Math.round(d/1000000) + "M" : 
            ( d/1000 > 1 ? Math.round(d/1000) + "K" : d + ""); 
    }
    function n2d(n) { return n < 10 ? "0" + n : "" + n; }

    /*
        Callbacks
    */

    $.app.add_pnode = function() {
        var v = $("#add_pnode_ip").val();
        if(v && v.length && RE_IP.test(v)) {
            $("#add_pnode_ip").attr("disabled", "disabled"); 
            req({op: "add_pnode", ip: v}); 
        } 
        return false;
    }
    function ui_add_pnode_reply(obj) {
        $("#add_pnode_ip").removeAttr("disabled"); 
        if(obj.result == "ok") $("#add_pnode_ip").val("");
    }

    $.app.drop_pnode = function(id) {
        if(confirm($("#confirm_drop_pnode").html())) { 
            $("#" + ui_id("pnode", id)).fadeOut('fast');
            req({op: "drop_pnode", id: id}); 
        } 
    }
    function ui_drop_pnode_reply(obj) { ui_detach("pnode", obj, ""); }

    /*
        Scenario
    */

    function ui_scenarios(d) {
        switch(d.ct) {
            case "list": ui_scenarios_list(d.v); break;
            case "item": show_scenario_params(d.v); break;
            case "item_edit": paste_scenario(d.v); break;
            case "update": 
                $("#se_save_btn").removeClass("disabled");
                $("#se_drop_btn").removeClass("disabled");
                $(".sc_" + se_current.id).detach();
                cleanup_scenario();
                break;
        }
    }
    function ui_scenarios_list(d) {
        if(d && d.length) {
            $("#scenarios_list > option").detach();
            $("#scenarios_editor_list > .sc").detach();
            $.map(d, function(v, _k) {
               $("#scenarios_list").append(ui_apply_template("sc_item_list", v));

                var ie = $(ui_apply_template("sc_item_editor", v));
                ie.click(function() { select_scenario(this); });
                $("#scenarios_editor_list").append(ie);
            });
        }
    }

    function ui_datasource(d) {} 

    function ui_exec(d) { 
        switch(d.t) {
            case "start":
                $("#console_" + d.id).append(ui_apply_template("console_cmd", {v: d.cmd}));
                break; 
            case "console":
                var el = $("#console_" + d.id);
                el.append(strip_html(d.v));
                el[0].scrollTop = el[0].scrollHeight;
                break; 
            case "done":
                $("#run_btn_" + d.id).hide();
                req({op: "scenarios"});
                notify('done', { node: $("#panel_" + d.id + " h2").text() });
                break; 
        }
    } 

    $.app.apply_scenario = function() {
        var sc = $("#scenarios_list").val();
        var ns = $("#nodes_list").val();

        if(!sc || sc.length == 0) return ui_mark("#scenarios_list", 1);
        if(!ns || ns.length == 0) return ui_mark("#nodes_list", 1);

        scenario.id = parseInt(sc[0]);
        scenario.hosts = ns; 

        req({op: "scenario", id: scenario.id});

        $("#apply_scenario_btn").addClass("disabled");
        $("#scenario_editor_btn").addClass("disabled");
        $("#cancel_scenario_btn").removeClass("disabled");
        $("#scenarios_list").attr("disabled", "disabled");
        $("#nodes_list").attr("disabled", "disabled");
    }

    $.app.cancel_scenario = function() {
        $("#apply_scenario_btn").removeClass("disabled");
        $("#scenario_editor_btn").removeClass("disabled");
        $("#cancel_scenario_btn").addClass("disabled");
        $("#scenarios_list").removeAttr("disabled");
        $("#nodes_list").removeAttr("disabled");

        $("#consoles > *").detach();

        scenario.id = 0;
        scenario.hosts = [];
        scenario.v = {}; 
    }

    function show_scenario_params(d) {
        if(scenario.hosts.length) {
            var pc = "";
            var fe = [];

            scenario.v = d;
            $.each(d.params, function(_i, v) {
                var t = v.type.split(":");

                if(t.length > 1) { 
                    if(t[0] == "Data") fe.push({ op: "datasource", type: t[1] }); 
                };
                pc = pc + ui_apply_template("_form_" + t[0], v); 
            });
            $.each(scenario.hosts, function(_i, v) {
                var title = $("option[value=" + v + "]").text();
                var node = v.split("_");
                var c = $(ui_apply_template("host_console", {
                        id: v,
                        title: title,
                        pnode: node[0], 
                        vnode: node[1]  
                    }).replace('%params_code%', pc));
                (function(cid, pid, vid) { 
                        $(".exec", c).click(function() { exec(cid, pid, vid); }); 
                    })(v, node[0], node[1]);
                $("#consoles").append(c);
                $.each(fe, function(_ii, vv) { 
                        vv.id = v; 
                        vv.pnode = node[0]; 
                        vv.vnode = node[1]; 
                        req(vv); 
                    });
            }); 
        };
    }

    function exec(cid, pid, vid) {
        var p = $("#panel_" + cid); 
        var cmds = scenario.v.commands;

        $.each(scenario.v.params, function(_i, v) {
                var vl = $(".p" + v.num, p).val();
                cmds = cmds.replace(new RegExp("\\$" + v.num, 'g'), vl);                
            });

        $("input", p).attr("disabled", "disabled");
        $("select", p).attr("disabled", "disabled");
        $("#run_btn_" + cid).addClass("disabled");

        req({ op: "exec",
              id: cid, 
              scenario: scenario.id,
              pnode: pid, 
              vnode: vid,
              cmds: cmds 
            });
    }

    /*
        Scenario editor
    */

    $.app.se_new_scenario = function() {
        if($("#scenario_commands").val().length && !confirm($("#confirm_drop_scenario").text())) return;
        cleanup_scenario();
    }

    function cleanup_scenario() {
        se_current = {id: 0, name: "", params: [], commands: ""};
        se_max_n = 1;

        $("#scenario_id").text("!");
        $("#scenario_name").val("");
        $("#scenario_params > *").detach();
        $("#scenario_commands").val("");
    }       

    function select_scenario(self) {
        req({op: "scenario_edit", id: parseInt($(self).attr("rel"))});

        $("#se_scenario_select").addClass("disabled");
        $("#se_save_btn").addClass("disabled");
        $("#se_drop_btn").addClass("disabled");        
    }

    function paste_scenario(d) {
        $("#se_scenario_select").removeClass("disabled");
        $("#se_save_btn").removeClass("disabled");
        $("#se_drop_btn").removeClass("disabled");

        cleanup_scenario();

        se_current = {id: d.id, name: d.name, params: d.params, commands: d.commands};

        $("#scenario_id").text(d.id);
        $("#scenario_name").val(d.name);
        $("#scenario_commands").val(d.commands);

        if($.isArray(d.params)) {
            $.each(d.params, function(_k, v) {
                if(v.num > se_max_n) se_max_n = v.num;
                var code = append_param(v);
                $("div.type_value", code).text(v.type);
            });            
        } else se_current.params = [];
        se_max_n++;
    }

    $.app.se_add_param = function() {
        var i = { num: se_max_n++, title: "", type: "undefined"};
        se_current.params.push(i);
        append_param(i);
    }
    function append_param(i) {
        var code = $(ui_apply_template('sc_param', i));
        $(".ui.dropdown", code).dropdown();
        $(".menu .item", code).click(function() { se_set_param_type(this); }); 
        $(".title", code).change(function() { se_update_param(this); });
        $(".dropme", code).click(function() { se_drop_param(this); }); 
        $("#scenario_params").append(code);
        return code;
    }
    function se_update_param(self) {
        var top = ui_up($(self), 2);
        var t = $(".title", top).val();    
        var num = $(".num", top).text();
        se_current.params = $.map(se_current.params, function(v, k) { if(num == v.num) v.title = t; return v; });
    }
    function se_drop_param(self) {
        var top = ui_up($(self), 2);
        var r = [];
        var num = $(".num", top).text();
        $.each(se_current.params, function(_k, v) { if(v.num != num) r.push(v); });
        se_current.params = r;
        top.hide();
    }
    function se_set_param_type(self) {
        var s = $(self);
        var top = ui_up(s, 4);
        var num = $(".num", top).text();
        var t = s.text();
        se_current.params = $.map(se_current.params, function(v, k) { if(num == v.num) v.type = t; return v; });
        $(".type_value", top).text(t);
    }
    
    $.app.se_save = function() {
        se_current.name = $("#scenario_name").val();
        se_current.commands = $("#scenario_commands").val();

        /* model check */
        if(se_current.name.length == 0) return ui_mark("#scenario_name", 1);
        if(se_current.commands.length == 0) return ui_mark("#scenario_commands", 1);
        var valid = true;
        $.map(se_current.params, function(v, _k) { 
                if(v.title.length == 0) { ui_mark(".sc_param_" + v.num + " .title", 1); valid = valid && false; };
                if(v.type == "undefined") { ui_mark(".sc_param_" + v.num + " .type_value", 1); valid = valid && false; };
            });
        if(!valid) return;

        /* send to server */
        req({op: "scenario_update", obj: se_current}); 
        $("#se_save_btn").addClass("disabled");
        $("#se_drop_btn").addClass("disabled");
    }
    $.app.se_drop = function() {
        if(!confirm($("#confirm_drop_scenario").text())) return;
        if(se_current.id > 0) { 
            req({op: "scenario_drop", id: se_current.id}); 
            $(".sc_" + se_current.id).detach(); 
        };
        cleanup_scenario();
    }

    /*
        WebRTC
    */

    function ui_peers(obj) { if(obj && obj.count) $("#peers").html(obj.count); }
    function ui_call(obj) { 
        if(!call.can) return;
        if(!obj || !obj.s || !obj.from) return;

        if(call.on) {
            switch(obj.s) {
                case 'ask':
                    if(!call.peers[obj.from]) offer_peer(obj.from);
                    break;
                case 'offer':
                    remote_peer(obj.from, obj.desc);
                    answer_peer(obj.from);
                    break;
                case 'answer':
                    remote_peer(obj.from, obj.desc);
                    break;
                case 'ice':
                    ice_peer(obj.from, obj);
                    break;
                case 'drop':
                    drop_peer(obj.from);
                    break;
            }
        } else {
            switch(obj.s) {
                case 'ask':
                    $("#call_answer").unbind('click').click(function() {
                        ui_init_call(obj.from);
                        $("#peers").popup('hide');
                    });
                    $("#call_reject").unbind('click').click(function() {
                        $("#peers").popup('hide');
                    });
                    $("#peers").popup('show');   
                    notify('call', {});                 
                    break;
            }
        };
    }
 
    function ui_init_call(remote) {
        if(call.can) {
            $("#call_panel")
                .modal({
                    onVisible: function() { ui_begin_call(remote); },
                    onHide: function() { ui_end_call(); }
                })
                .modal('show');
        } else {
            $("#peers").addClass("disabled");
        }
    }

    function ui_begin_call(remote) {
        call.on = true;
        navigator.getUserMedia(
            { audio: true, video: true }, 
            function(stream) {
                $("#video_self").attr("src", URL.createObjectURL(stream));
                call.stream = stream;

                if(remote) offer_peer(remote);
                else req({ op: 'call', to: 'all', s: 'ask' });
            }, 
            function(error) {
                $("#um_recall").unbind('click').click(function() { 
                    $("#um_error").transition('fade');
                    ui_begin_call(); });
                $("#um_error").transition('fade up');
                call.stream = null;
            });
    }
    function ui_end_call() {
        if(call.on) {
            call.on = false;
            req({ op: 'call', to: 'all', s: 'drop' });
            $("#streams .streams").detach();
            $.each(call.peers, function(k, _v) { call.peers[k] = null; });
        }
    }

    function con_peer(remote) {
        call.peers[remote] = new call.PC({"iceServers": [
                {"url": "stun:stun.l.google.com:19302"},
                {"url": "stun:stun1.l.google.com:19302"},
                {"url": "stun:stun2.l.google.com:19302"},
                {"url": "stun:stun3.l.google.com:19302"},
                {"url": "stun:stun4.l.google.com:19302"},
                {"url": "stun:stun.ekiga.net"}
            ]});
        call.peers[remote].addStream(call.stream);
        call.peers[remote].onicecandidate = function(event) {
            if(event && event.candidate) req({
                    op: "call",
                    to: remote,
                    s: "ice",
                    label: event.candidate.sdpMLineIndex,
                    id: event.candidate.sdpMid,
                    candidate: event.candidate.candidate
                });
        };
        call.peers[remote].onaddstream = function(event) {
            var v = $(ui_apply_template("stream", {id: remote}));
            $("video", v).attr("src", URL.createObjectURL(event.stream));
            $("#streams").append(v);  
        };
    }

    function offer_peer(id) {
        if(!call.peers[id]) con_peer(id);
        call.peers[id].createOffer(
                function(desc) { 
                    call.peers[id].setLocalDescription(desc); 
                    req({ 
                        op: "call", 
                        to: id, 
                        s: "offer", 
                        desc: desc }); },
                function(error) { console.log(error) }, 
                { 'mandatory': { 'OfferToReceiveAudio': true, 'OfferToReceiveVideo': true } }                    
            );
    }

    function answer_peer(id) {
        if(!call.peers[id]) return;
        call.peers[id].createAnswer(
                function(desc) { 
                    call.peers[id].setLocalDescription(desc); 
                    req({ 
                        op: "call", 
                        to: id, 
                        s: "answer",
                        desc: desc }); },
                function(error) { console.log(error) }, 
                { 'mandatory': { 'OfferToReceiveAudio': true, 'OfferToReceiveVideo': true } }                    
            );
    }

    function remote_peer(id, desc) {
        if(!call.peers[id]) con_peer(id);
        call.peers[id].setRemoteDescription(new call.SD(desc));
    }

    function ice_peer(id, obj) {
        if(!call.peers[id]) return;
        var icec = new call.ICE({ sdpMLineIndex: obj.label, candidate: obj.candidate });
        call.peers[id].addIceCandidate(icec);
    }

    function drop_peer(id) { 
        $("#video_" + id).detach();
        call.peers[id] = null;
    }

    /*
        Notifications
    */

    function notification_init() {
        if(Notification && Notification.requestPermission) {
            notification_state = "default"; 
            Notification.requestPermission(function(r) { notification_state = r.toLowerCase(); });
        }
    }

    function notification_make(title, body, tag, icon, click_cb) {
        if(notification_state != "granted") return;
    
        var n = new Notification(title, {
                tag : tag,
                body : body,
                icon : icon
            });
        n.onclick = function() { click_cb(tag); };
        (function (nn) { 
                setTimeout(function() { if(nn) nn.close(); }, 8000); 
            })(n);
    }

    function notify(tag, params) {
        if(notification_state != "granted") return;

        var title = ui_apply_template("n_" + tag + "_title", params);
        var body = ui_apply_template("n_" + tag + "_body", params);
        var icon = ui_apply_template("n_" + tag + "_icon", params);

        notification_make(title, body, tag, icon, notify_cb);
    }

    function notify_cb(tag) {
        switch(tag) {
            case 'call': 
                $("#call_answer").trigger('click');
                break;
            case 'done':
                $("#_scenarios").trigger('click');
                break;
        }
    }


    /* 
        Utils
    */

    function strip_html(str) {
        str = str ? "" + str : "";
        $.each([["&", "&amp;"], ["<", "&lt;"], [">", "&gt;"], ["\n", "<br/>"]], function(_k, v) { 
                str = str.replace(new RegExp(v[0], 'g'), v[1]); 
            });
        return str;
    }

})(window.jQuery);

$(document).ready(function() { $.app.init(); });