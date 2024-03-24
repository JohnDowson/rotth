use std::{collections::HashMap, path::PathBuf, sync::Arc};

use axum::{extract::Query, http::StatusCode, routing::get, Extension, Json, Router};
use chisel::{ModuleContent, ModuleRequest};
use itempath::ItemPathBuf;
use tokio::{net::TcpListener, sync::RwLock};
use tower_http::trace::TraceLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> std::io::Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "chisel=debug,tower_http=debug,axum::rejection=trace".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let cwd = std::env::current_dir()?;
    let root = std::env::var("ROTTH_SRC")
        .map(PathBuf::from)
        .unwrap_or_else(|_| cwd.join("src"));
    let root = Arc::new(root);
    let cache: Arc<RwLock<HashMap<ItemPathBuf, ModuleContent>>> = Default::default();

    let app = Router::new()
        .route("/module", get(module))
        .layer(TraceLayer::new_for_http())
        .layer(Extension(cache))
        .layer(Extension(root));

    let listener = TcpListener::bind("localhost:4269").await?;
    tracing::debug!("listening on {}", listener.local_addr().unwrap());
    axum::serve(listener, app).await
}

async fn module(
    Query(ModuleRequest { path: request }): Query<ModuleRequest>,
    Extension(root): Extension<Arc<PathBuf>>,
    Extension(cache): Extension<Arc<RwLock<HashMap<ItemPathBuf, ModuleContent>>>>,
) -> (StatusCode, Json<Result<ModuleContent, String>>) {
    let cache_read = cache.read().await;
    let content = match cache_read.get(&request) {
        Some(c) => c.clone(),
        None => {
            let mut path = request
                .iter()
                .fold((*root).clone(), |acc, frag| acc.join(frag.as_str()));
            path.set_extension("rh");

            let Ok(contents) = tokio::fs::read_to_string(&path).await else {
                return (
                    StatusCode::NOT_FOUND,
                    Json(Err(path.to_string_lossy().to_string())),
                );
            };
            let content = ModuleContent {
                path,
                module_path: request.clone(),
                contents,
            };
            drop(cache_read);
            cache.write().await.insert(request, content.clone());
            content
        }
    };

    (StatusCode::OK, Json(Ok(content)))
}
