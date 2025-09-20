import {AUTH_TOKENS} from "@/util/constants";
import {decodeBase64Str, getOAuthIssuer} from "@/util/tool";
import {fetchEventSource} from "@microsoft/fetch-event-source";
import {onUnmounted} from "vue";

interface EventSourceOptions {
  url: string;
  method?: "GET" | "POST";
  headers?: Record<string, string>;
  body?: any;
  onMessage?: (data: string) => void;
  onOpen?: (response: Response) => void;
  onError?: (err: any) => void;
  onClose?: () => void;
}

export function userEventSource() {
  let controller: AbortController | null = null;

  const fetchStream = async (options: EventSourceOptions) => {
    controller = new AbortController();
    const headers = {
      "Content-Type": "application/json",
    };
    const authTokens = localStorage.getItem(AUTH_TOKENS);
    if (authTokens) {
      const token: any = JSON.parse(decodeBase64Str(authTokens));
      headers["Authorization"] = `${token.token_type} ${token.access_token}`;
    }
    const baseUrl = `${getOAuthIssuer()}${import.meta.env.VITE_API_BASE_URI}`;

    await fetchEventSource(baseUrl + options.url, {
      method: options.method || "POST",
      headers: {
        ...headers,
        ...options.headers,
      },
      body: options.body ? JSON.stringify(options.body) : undefined,
      signal: controller.signal,
      onopen: async (response) => {
        if (response.ok) {
          options.onOpen?.(response);
          return;
        }
        throw new Error(
          `Failed to open stream: ${response.status} ${response.statusText}`
        );
      },
      onmessage: (event) => {
        if (event.data) {
          try {
            const jsonData = JSON.parse(event.data);
            options.onMessage?.(jsonData);
          } catch (err) {
            options.onMessage?.(event.data);
          }
        }
      },
      onerror: (err) => {
        options.onError?.(err);
        throw err;
      },
      onclose: () => {
        options.onClose?.();
      },
    });
  };

  const abort = () => {
    if (controller) {
      controller.abort();
      controller = null;
    }
  };

  onUnmounted(() => {
    abort();
  });

  return {
    fetchStream,
    abort,
  };
}
