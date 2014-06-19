package edu.kufpg.armatus.networking;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Optional;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NoHttpResponseException;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.conn.HttpHostConnectException;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;

/**
 * Task that connects to a server running HERMIT-web and simulates HERMIT commands
 * by using HTTP GET and POST requests.
 */
public abstract class HermitHttpServerRequest<Result> extends AsyncActivityTask<ConsoleActivity, String, Void, Result> {
    private final HttpRequest mRequest;
    private Optional<String> mErrorMessage = Optional.absent();

    /**
     * Constructs a new instance.
     *
     * @param console reference to the current console.
     */
    public HermitHttpServerRequest(@NonNull final ConsoleActivity console,
                                   @NonNull final HttpRequest request) {
        super(console);
        mRequest = request;
    }

    @Override protected void onPreExecute() {
        super.onPreExecute();

        getActivity().setProgressBarVisibility(true);
        getActivity().disableInput(true);
    }

    @Override protected Result doInBackground(@Nullable final String... params) {
        HttpClient httpClient = null;
        final HttpResponse httpResponse;
        String responseStr = null;

        try {
            final HttpParams httpParams = new BasicHttpParams();
            //Set timeout length to 10 seconds
            HttpConnectionParams.setConnectionTimeout(httpParams, 10000);
            HttpConnectionParams.setSoTimeout(httpParams, 10000);
            httpClient = new DefaultHttpClient(httpParams);
            final HttpUriRequest request;

            final String url;
            if (params != null && params[0] != null) {
                url = params[0];
            } else {
                return cancelResult(null, "No URL specified.");
            }

            if (mRequest == HttpRequest.GET) {
                request = new HttpGet(url);
            } else { // if (mRequest == HttpRequest.POST) {
                final HttpPost httpPost = new HttpPost(url);
                if (params.length > 1) {
                    httpPost.setHeader("content-type", "application/json");
                    if (params[1] != null && !params[1].isEmpty()) {
                        try {
                            httpPost.setEntity(new StringEntity(params[1]));
                        } catch (final UnsupportedEncodingException e) {
                            e.printStackTrace();
                        }
                    }
                }
                request = httpPost;
            }

            if (!isCancelled()) {
                httpResponse = httpClient.execute(request);
                if (httpResponse == null) {
                    return cancelResult(null, "No response.");
                }
            } else {
                return null;
            }

            final StatusLine statusLine = httpResponse.getStatusLine();
            if (statusLine == null) {
                return cancelResult(null, "No status.");
            }

            final HttpEntity httpEntity = httpResponse.getEntity();
            if (httpEntity == null) {
                return cancelResult(null, "No HTTP entity.");
            }

            if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
                responseStr = EntityUtils.toString(httpEntity, HTTP.UTF_8).trim();
            } else {
                return cancelResult(null, "Error code " + statusLine.getStatusCode());
            }
        } catch (final HttpHostConnectException e) {
            return cancelResult(e, "ERROR: server connection refused.");
        } catch (final ClientProtocolException e) {
            return cancelResult(e, "ERROR: client protocol problem.");
        } catch (final NoHttpResponseException e) {
            return cancelResult(e, "ERROR: the target server failed to respond.");
        } catch (final ConnectTimeoutException e) {
            return cancelResult(e, "ERROR: the server connection timed out.");
        } catch (final SocketTimeoutException e) {
            return cancelResult(e, "ERROR: the server connection timed out.");
        } catch (final IOException e) {
            return cancelResult(e, "ERROR: I/O problem.");
        } finally {
            if (httpClient != null) {
                httpClient.getConnectionManager().shutdown();
            }
        }

        return onResponse(responseStr);
    }

    @Override protected void onPostExecute(@Nullable final Result result) {
        super.onPostExecute(result);

        end();
    }

    @Override protected void onCancelled(@Nullable final Result error) {
        super.onCancelled(error);

        if (mErrorMessage.isPresent() && getActivity() != null) {
            getActivity().appendErrorResponse(mErrorMessage.get());
        }
        end();
    }

    private void end() {
        if (getActivity().getHermitClient().isRequestDelayed()) {
            getActivity().getHermitClient().notifyDelayedRequestFinished();
        }

        getActivity().enableInput();
        getActivity().setProgressBarVisibility(false);
    }

    @Nullable private Result cancelResult(@Nullable final Exception error,
                                @Nullable final String errorMsg) {
        if (error != null) {
            error.printStackTrace();
        }
        setErrorMessage(errorMsg);
        cancel(true);
        return null;
    }

    @Nullable protected abstract Result onResponse(@NonNull final String response);

    @NonNull protected Optional<String> getErrorMessage() {
        return mErrorMessage;
    }

    protected void setErrorMessage(@Nullable final String message) {
        mErrorMessage = Optional.fromNullable(message);
    }

    public enum HttpRequest { GET, POST }

}
