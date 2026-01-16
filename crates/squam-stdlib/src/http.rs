use squam_vm::{Value, VM};
use std::collections::HashMap;
use std::rc::Rc;
use squam_vm::value::StructInstance;

fn create_response_struct(status: i64, body: String, headers: HashMap<String, String>) -> Value {
    // Create headers struct with dynamic fields
    let headers_instance = StructInstance::new_dynamic("Headers".to_string());
    for (k, v) in headers {
        headers_instance.fields().borrow_mut().insert(k, Value::String(Rc::new(v)));
    }

    // Create response struct with ordered fields
    let field_names = vec!["status".to_string(), "body".to_string(), "headers".to_string()];
    let field_values = vec![
        Value::Int(status),
        Value::String(Rc::new(body)),
        Value::Struct(Rc::new(headers_instance)),
    ];
    Value::Struct(Rc::new(StructInstance::new("Response".to_string(), field_names, field_values)))
}

pub fn register(vm: &mut VM) {
    // http_get(url: string) -> Response { status: int, body: string, headers: struct }
    vm.define_native("http_get", 1, |args| {
        match &args[0] {
            Value::String(url) => {
                match ureq::get(url.as_str()).call() {
                    Ok(response) => {
                        let status = response.status() as i64;
                        let mut headers = HashMap::new();
                        for name in response.headers_names() {
                            if let Some(value) = response.header(&name) {
                                headers.insert(name, value.to_string());
                            }
                        }
                        let body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(status, body, headers))
                    }
                    Err(ureq::Error::Status(code, response)) => {
                        let body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(code as i64, body, HashMap::new()))
                    }
                    Err(e) => Err(format!("http_get failed: {}", e)),
                }
            }
            _ => Err("http_get: expected string url".to_string()),
        }
    });

    // http_get_text(url: string) -> string (simple version that just returns body)
    vm.define_native("http_get_text", 1, |args| {
        match &args[0] {
            Value::String(url) => {
                match ureq::get(url.as_str()).call() {
                    Ok(response) => {
                        let body = response.into_string().unwrap_or_default();
                        Ok(Value::String(Rc::new(body)))
                    }
                    Err(ureq::Error::Status(_, response)) => {
                        let body = response.into_string().unwrap_or_default();
                        Ok(Value::String(Rc::new(body)))
                    }
                    Err(e) => Err(format!("http_get_text failed: {}", e)),
                }
            }
            _ => Err("http_get_text: expected string url".to_string()),
        }
    });

    // http_post(url: string, body: string) -> Response
    vm.define_native("http_post", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(url), Value::String(body)) => {
                match ureq::post(url.as_str())
                    .set("Content-Type", "application/json")
                    .send_string(body.as_str())
                {
                    Ok(response) => {
                        let status = response.status() as i64;
                        let mut headers = HashMap::new();
                        for name in response.headers_names() {
                            if let Some(value) = response.header(&name) {
                                headers.insert(name, value.to_string());
                            }
                        }
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(status, resp_body, headers))
                    }
                    Err(ureq::Error::Status(code, response)) => {
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(code as i64, resp_body, HashMap::new()))
                    }
                    Err(e) => Err(format!("http_post failed: {}", e)),
                }
            }
            _ => Err("http_post: expected (string, string)".to_string()),
        }
    });

    // http_post_json(url: string, body: string) -> string (simple version)
    vm.define_native("http_post_json", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(url), Value::String(body)) => {
                match ureq::post(url.as_str())
                    .set("Content-Type", "application/json")
                    .send_string(body.as_str())
                {
                    Ok(response) => {
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(Value::String(Rc::new(resp_body)))
                    }
                    Err(ureq::Error::Status(_, response)) => {
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(Value::String(Rc::new(resp_body)))
                    }
                    Err(e) => Err(format!("http_post_json failed: {}", e)),
                }
            }
            _ => Err("http_post_json: expected (string, string)".to_string()),
        }
    });

    // http_put(url: string, body: string) -> Response
    vm.define_native("http_put", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(url), Value::String(body)) => {
                match ureq::put(url.as_str())
                    .set("Content-Type", "application/json")
                    .send_string(body.as_str())
                {
                    Ok(response) => {
                        let status = response.status() as i64;
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(status, resp_body, HashMap::new()))
                    }
                    Err(ureq::Error::Status(code, response)) => {
                        let resp_body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(code as i64, resp_body, HashMap::new()))
                    }
                    Err(e) => Err(format!("http_put failed: {}", e)),
                }
            }
            _ => Err("http_put: expected (string, string)".to_string()),
        }
    });

    // http_delete(url: string) -> Response
    vm.define_native("http_delete", 1, |args| {
        match &args[0] {
            Value::String(url) => {
                match ureq::delete(url.as_str()).call() {
                    Ok(response) => {
                        let status = response.status() as i64;
                        let body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(status, body, HashMap::new()))
                    }
                    Err(ureq::Error::Status(code, response)) => {
                        let body = response.into_string().unwrap_or_default();
                        Ok(create_response_struct(code as i64, body, HashMap::new()))
                    }
                    Err(e) => Err(format!("http_delete failed: {}", e)),
                }
            }
            _ => Err("http_delete: expected string url".to_string()),
        }
    });
}